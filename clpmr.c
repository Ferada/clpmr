/*  clpmrc.c --- client of clpmr

 Copyright (C) 2004-2008 by Walter C. Pelissero

 Author: Walter C. Pelissero <walter@pelissero.de>
 Project: CLPMR a Procmail replacement in CL
*/

static char RCSid[] = "$Module: clpmr.c, Time-stamp: <2006-12-02 18:29:23> $";

/*
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2, or (at
your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 Commentary:

The clpmr client is the little program that is called by sendmail (or
any other MTA) in the last phase of the delivery of a mail message.
Supposed the user has set up the ~/.forward file to call clpmr.  Clpmr
itself does very little; it runs a clpmr server (written in Common
Lisp) if necessary, and forward to the running clpmr server the whole
message.  Nothing else.

The server is responsible of actually processing the message. */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/un.h>
#include <errno.h>
#include <pwd.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>

#include "locking.h"

#define WAIT_FOR_SERVER	30 /* seconds to wait the clpmr server to come up */
#define SOCKET_NAME	".clpmr.server"
#define CONF_FILE	".clpmr.conf"
#ifdef USE_LOCK_FILES
# define LOCK_FILE	".clpmr.lock"
# define LOCK_EXPIRATION (2 * 60 * 60) /* two hours */
#endif

static char *myname;		/* set in main() */
static int verbose = 0, debug = 0;
static time_t timeout = WAIT_FOR_SERVER;

static struct passwd *
pw_data ()
{
  static struct passwd *pw;
  static uid_t uid;

  if (pw)
    return pw;
  uid = getuid();
  pw = getpwuid(uid);
  if (!pw)
    {
      fprintf(stderr, "%s: uid %d not present in passwd file?!\n", myname, uid);
      exit(5);
    }
  endpwent();
  return pw;
}

/* load_user_data makes sure that some environment variables are
   properly set, because programs run by sendmail may have a rather
   depleted environment and the server process may depend on them. */
static void
load_user_data ()
{
  setenv("USER", pw_data()->pw_name, 0);
  setenv("HOME", pw_data()->pw_dir, 0);
}

static const char *
socket_path ()
{
  static char buf[1024];

  if (!buf[0])
    sprintf(buf, "%s/%s", getenv("HOME"), SOCKET_NAME);
  return buf;
}

#ifdef USE_LOCK_FILES
static const char *
lock_path ()
{
  static char buf[1024];

  if (!buf[0])
    sprintf(buf, "%s/%s", getenv("HOME"), LOCK_FILE);
  return buf;
}
#endif

static const char *
conf_path ()
{
  static char buf[1024];

  if (!buf[0])
    sprintf(buf, "%s/%s", getenv("HOME"), CONF_FILE);
  return buf;
}

static int
remove_socket ()
{
  return unlink(socket_path());
}

static bool
file_exists (const char *path)
{
  int old_errno = errno;
  int result;

  result = access(path, F_OK);
  errno = old_errno;
  return result == 0;
}

static int
connect_to_server (time_t wait)
{
  int sock;

  if ((sock = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0)
    fprintf(stderr, "%s: can't get a socket (errno=%d)\n", myname, errno);
  else
    {
      struct sockaddr_un serv_addr;
      int servlen;
      time_t end;

      /* blank the address and fill it */
      memset(&serv_addr, 0, sizeof(serv_addr));
      serv_addr.sun_family = PF_LOCAL;
      strcpy(serv_addr.sun_path, socket_path());
      servlen = SUN_LEN(&serv_addr) + 1;

      end = time(0) + wait;
      while (connect(sock, (struct sockaddr *)&serv_addr, servlen) < 0)
	{
	  if (errno != ECONNREFUSED ||
	      !file_exists(socket_path()))
	    {
	      if (verbose)
		fprintf(stderr, "%s: can't connect to socket \"%s\" (errno=%d).\n",
			myname, socket_path(), errno);
	      close(sock);
	      return -1;
	    }
	  if (!(wait && time(0) < end))
	    {
	      if (verbose)
		fprintf(stderr, "%s: can't connect to socket \"%s\": waited too long.\n",
			myname, socket_path());
	      close(sock);
	      return -1;
	    }
	  /* The server socket is actually there but the server is
	     refusing connection probably because it's busy crunching
	     other messages.  We keep trying after one second
	     pause. */
	  sleep(1);
	}
    }
  return sock;
}


static int
spawn_new_server ()
{
  int child = fork();

  if (child == 0)
    {				/* this is the child process */
      char image[1024];

      if (setsid() < 0)
	fprintf(stderr, "%s: cannot create new process group (errno=%d)\n", myname, errno);
      sprintf(image, "%s.server", myname);
      /* Unless we want to be verbose, shut up the server which tends
	 to be a chatterbox. */
      if (!verbose)
	{
	  close(1);
	  close(2);
	  open("/dev/null", O_WRONLY);
	  dup(1);
	}
      if (debug)
	execlp(image, image, "-d", (char *)0);
      else
	execlp(image, image, (char *)0);
      fprintf(stderr, "%s: unable to run %s (errno=%d)\n", myname, image, errno);
      exit(1);
    }
  else if (child < 0)
    {
      fprintf(stderr, "%s: unable to fork (errno=%d)\n", myname, errno);
      exit(2);
    }
  /* this is the parent process */
  return child;
}


static void
dowrite (int fd, const char *buf, int length)
{
  int written = 0;

  while (written < length)
    {
      int n = write(fd, buf + written, length - written);

      if (n < 0)
	{
	  fprintf(stderr, "%s: write error on %d (errno=%d)\n", myname, fd, errno);
	  exit(3);
	}
      written += n;
    }
}

static void
pass_message_to_server (int server_socket)
{
  char buf[1024];
  int n;

  while ((n = read(0, buf, sizeof(buf))) > 0)
    dowrite(server_socket, buf, n);
}

#ifdef USE_LOCK_FILES
static bool locked = false;

static void
remove_lock ()
{
  if (locked)
    delete_lock_file(lock_path());
}

static void
create_lock ()
{
  if (!locked)
    {
      create_lock_file(lock_path(), 7, -1, 13, LOCK_EXPIRATION, 0);
      locked = true;
    }
}
#else				/*  USE_LOCK_FILES */
static int lock_fd = -1;

static void
remove_lock ()
{
  if (lock_fd >= 0)
    {
      flock(lock_fd, LOCK_UN);	/* redundant */
      close(lock_fd);
    }
}

static void
create_lock ()
{
  if (lock_fd < 0)
    lock_fd = open(conf_path(), O_RDONLY);
  if (lock_fd < 0)
    {
      fprintf(stderr, "%s: unable to open %s\n", myname, conf_path());
      exit(7);
    }
  else
    flock(lock_fd, LOCK_EX);
}
#endif				/*  USE_LOCK_FILES */

static void
usage ()
{
  fprintf(stderr, "%s\n", RCSid);
  fprintf(stderr, "usage: %s [-d][-v][-t timeout] < mail_message\n", myname);
  exit(1);
}

static void
signal_handler (int signal)
{
  remove_lock();
  exit(6);
}


int
main (int argc, char *argv[])
{
  int socket;
  char c;

  myname = argv[0];
  while ((c = getopt(argc, argv, "vdt:")) != -1)
    {
      switch (c)
	{
	case 'd':
	  ++debug;
	  /* fall through */
	case 'v':
	  ++verbose;
	  break;
	case 't':
	  timeout = atoi(optarg);
	  break;
	case '?':
	default:
	  usage();
	}
    }
  load_user_data();
  atexit(remove_lock);
  signal(SIGHUP, signal_handler);
  signal(SIGINT, signal_handler);
  signal(SIGQUIT, signal_handler);
  signal(SIGTERM, signal_handler);
  create_lock();
  if ((socket = connect_to_server(timeout)) < 0)
    {
      time_t end;

      remove_socket();
      spawn_new_server();
      /* a Lisp system may take a while to start up */
      sleep(2);
      for (end = time(0) + timeout; time(0) < end;)
	{
	  if ((socket = connect_to_server(0)) >= 0)
	    break;
	  sleep(1);
	}
      if (socket < 0)
	{
	  fprintf(stderr, "%s: the server failed to get ready in %ld seconds; giving up.\n",
		  myname, timeout);
	  exit(4);
	}
    }
  remove_lock();
  pass_message_to_server(socket);
  close(socket);
  return 0;
}
