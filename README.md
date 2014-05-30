====================
System configuration
====================
The programming language being used is Haskell, a purely
functional programming language. The particular implementation 
of Haskell used for this system is the Glasgow Haskell
Compiler v.7.4.2 on Linux.

The database management system of choice is the lightweight
embedded DMBS SQLite3.

The system also makes use of some Haskell libraries:
• HDBC, the Haskell Database Connectivity library
• HDBC.SQLite3, SQLite3’s backend for HDBC. This library implies
the presence of
Sqlite3.
• Readline, for a little better I/O in the commandline mode.
Likewise this library also requires readline already on the system
• Template Haskell, a metaprogramming library for Haskell

Most of these libraries can be installed along with the Haskell
Platform. To install the Haskell Platform on a Debian-based
system:
sudo apt-get install haskell-platform

To install HDBC and its SQLite3's back end"
sudo apt-get install libghc-hdbc-dev
sudo apt-get install libghc-hdbc-sqlite3-dev

=================
Using the program
=================
To use the program, in the source directory of the system, invoke
the following command:

runhaskell Main.hs <db-name>

Where <db-name> is the path to a sqlite3 database file. There is
a sample database named example.db in the source directory.

After that, the program will enter an interactive loop. From here
on the user can interact with the program by using command as
specified by the program. To see what you can do in the
interactive loop, use the command ``menu''.
