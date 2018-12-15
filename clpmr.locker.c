/*  clpmr.locker.c --- Create/remove lock semaphore files

 Copyright (C) 2006, 2007 by Walter C. Pelissero

 Author: Walter C. Pelissero <walter@pelissero.de>
 Project: CLPMR a Procmail replacement in CL
*/

static char RCSid[] = "$Module: clpmr.locker.c, Time-stamp: <2006-11-26 18:29:23 wcp> $";

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
Boston, MA 02111-1307, USA.*/

/* This program locks access to files writing a semaphore file with
   the same name as the file to be locked plus the extension ".lock".
   As a precaution against DoS attacks, care is taken not to lock
   files the user wouldn't have access to.

   BEWARE: on some systems this program must be installed sgid the
   "mail" group.  That's the reason why this stuff is not implemented
   in Lisp.  */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>

#include "locking.h"

static const char *myname;

enum exit_error_codes 
  {
    EXIT_OK,
    EXIT_BAD_USAGE,
    EXIT_NOT_LOCKABLE,
    EXIT_INTERNAL_ERROR
  };

static void
usage ()
{
  fprintf(stderr, "%s\n", RCSid);
  fprintf(stderr, "usage: %s [-s sleep_time] [-r retries] [-w wait_time_after_expire] [-e expiration_time] [-p pid] file_name\n", myname);
  fprintf(stderr, "       %s -u file_name\n", myname);
  exit(EXIT_BAD_USAGE);
}

int
main (int argc, char *argv[])
{
  char c, *file, lock_file[MAXPATHLEN];
  int sleep_time = 7;
  int retries = -1;
  int suspend = 13;
  int expiration = -1;
  int unlock = 0;
  pid_t pid = 0;

  myname = argv[0];

  while ((c = getopt(argc, argv, "e:p:r:s:uw:")) != -1)
    {
      switch (c)
	{
	case 'u':
	  unlock = 1;
	  break;
	case 'e':
	  expiration = atoi(optarg);
	  break;
	case 'p':
	  pid = atoi(optarg);
	  break;
	case 'r':
	  retries = atoi(optarg);
	  break;
	case 's':
	  sleep_time = atoi(optarg);
	  break;
	case 'w':
	  suspend = atoi(optarg);
	  break;
	case '?':
	default:
	  usage();
	}
    }
  if ((argc - optind) < 1)
    usage();
  file = argv[optind];
  if (access(file, R_OK) != 0)
    {
      if (errno == EACCES)
	{
	  fprintf(stderr, "%s: file %s cannot be locked or unlocked as it belongs to someone else.\n",
		  argv[0], file);
	  exit(EXIT_NOT_LOCKABLE);
	}
    }
  snprintf(lock_file, sizeof(lock_file), "%s.lock", file);
  if (argc < 2)
    usage();
  if (unlock)
    {
      if (!delete_lock_file(lock_file))
	{
	  fprintf(stderr, "%s: file %s cannot be unlocked (errno=%d).\n", myname, file, errno);
	  exit(EXIT_NOT_LOCKABLE);
	}
    }
  else
    {
      if (!create_lock_file(lock_file, sleep_time, retries, suspend, expiration, pid))
	{
	  fprintf(stderr, "%s: file %s cannot be locked (errno=%d).\n", myname, file, errno);
	  exit(EXIT_NOT_LOCKABLE);
	}
    }
  exit(EXIT_OK);
  return EXIT_INTERNAL_ERROR;
}
