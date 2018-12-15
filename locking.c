/*  locking.c --- functions to create/delete lock files

 Copyright (C) 2006, 2007 by Walter C. Pelissero

 Author: Walter C. Pelissero <walter@pelissero.de>
 Project: CLPMR a Procmail replacement in CL
*/

static char RCSid[] = "$Module: locking.c, Time-stamp: <2006-11-26 18:29:23 wcp> $";

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

/* This library locks access to files writing a semaphore file with
   the same name as the file to be locked plus the extension ".lock".
   As a precaution against DoS attacks, care is taken not to lock
   files the user wouldn't have access to. */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <signal.h>
#include <time.h>

#include "locking.h"

static time_t
modtime (const char *pathname)
{
  struct stat st;
  if (stat(pathname, &st) == 0)
    return st.st_mtime;
  return -1;
}

bool
delete_lock_file (const char *pathname)
{
  return unlink(pathname) == 0;
}

static bool
lock_owner_is_dead (const char *pathname)
{
  /* If we can read the lock file, we may be able to find out
     whether the owner of the lock is actually up and running or
     died abruptly forgetting the lock behind. */
  FILE *fp = fopen(pathname, "r");

  if (fp)
    {
      char buf[1024];

      if (fgets(buf, sizeof(buf), fp))
	{
	  int pid;
	  char *p;

	  fclose(fp);
	  buf[sizeof(buf) - 1] = '\0';
	  pid = strtol(buf, &p, 0);
	  if (p != buf && pid)
	    {
	      /* we found a pid in the lock file; let's see if the
		 owner is still alive */
	      if (kill(pid, 0) == 0)
		return false;	/* still alive */
	      else
		return true;	/* dead */
	    }
	}
      else
	fclose(fp);
    }
  /* if we can't look into the lock file we should assume the owner
     may still be alive */
  return false;
}

bool
create_lock_file (const char *pathname, int sleep_time, int retries, int suspend, int expiration, pid_t pid)
{
  if (lock_owner_is_dead(pathname))
    {
      /* the locking process has already died, we can
	 safely remove the lock */
      unlink(pathname);
    }
  if (expiration >= 0)
    {
      time_t epoch = modtime(pathname);

      if (epoch >= 0 &&
	  (epoch + expiration) < time(0))
	{
	  /* The lock is old enough that we can assume the owner might
	     be dead or stuck somewhere contemplating the meaning of
	     life. */
	  unlink(pathname);
	  sleep(suspend);
	}
    }
  {
    int i;

    for (i = 0; retries < 0 || i < retries; ++i)
      {
	int fd;

	if (i > 0)
	  sleep(sleep_time);
	fd = open(pathname, O_WRONLY | O_EXCL | O_CREAT, 0644);
	if (fd >= 0)
	  {
	    FILE *fp = fdopen(fd, "w");

	    fprintf(fp, "%d\n", (pid ? pid : getpid()));
	    fclose(fp);
	    return true;
	  }
      }
    return false;
  }
}

