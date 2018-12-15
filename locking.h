/*  locking.h --- interface of locking.h

 Copyright (C) 2006 by Walter C. Pelissero

 Author: Walter C. Pelissero <walter@pelissero.de>
 Project: CLPMR a Procmail replacement in CL
*/

/* $Module: locking.h, Time-stamp: <2006-11-26 18:29:23 wcp> $ */

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

typedef enum { false, true } bool;

extern bool delete_lock_file (const char *pathname);
extern bool create_lock_file (const char *pathname, int sleep_time, int retries, int suspend, int expiration, pid_t pid);
