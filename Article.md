# Double-Duty DOS

Jason Coleman

> Originally from Compute! Magazine, Issue 89 (October 1987)

_The Apple II has two popular operating systems, DOS 3.3 and ProDOS. If you've ever been trapped between them, you'll appreciate this utility-&mdash;it adds new commands to let you read and write DOS 3.3 files from within ProDOS, and move ProDOS files to DOS 3.3._

----

Most Apple users have two stacks of disks&mdash;one stack ProDOS and one stack DOS 3.3. That's not a problem until you want to use a program in one operating system that's written for the other one. Some people convert all of their 3.3 files to ProDOS format with a conversion utility. For the user with a large number of DOS 3.3 disks, this can be a trying experience. "Double-Duty DOS" offers a different approach&mdash;it lets you view DOS 3.3 catalogs and load and save BASIC and machine language programs, all from within ProDOS.

> Instructions for typing in the program are included here in the original article.

## A Door To The Old World

Let's give Double-Duty DOS a test drive. Boot up in ProDOS and `BRUN` the program. In addition to the normal commands that are available in ProDOS, you have several new ones designed especially for accessing DOS 3.3 disks.

Here is a list of the new commands:

**DCAT** _**,S**s **,D**d_

**DLOAD filename** _**,A**memloc **,S**s **,D**d_

**DSAVE filename** _**,S**s **,D**d_

**DSAVE filename, A$hhhh, L$hhhh** _**,S**s **,D**d_

Parameters in _italics_ are optional. The _s_ stands for slot number, and _d_ is for drive number. Place a DOS 3.3 disk in the drive and type `DCAT`. You'll see a list of all the files on the disk. You can then load any of these DOS 3.3 files using the `DLOAD` command, and save them to a ProDOS disk with the standard `SAVE` or `BSAVE` commands.

In the `DLOAD` command, the load address can be specified as a decimal or hexadecimal number. Precede hexadecimal values with a $. For example, `DLOAD TEST,S6,D0,A$900` loads a DOS 3.3 binary file named `TEST` into location $900 from slot 6, drive 0.

You can also move files from ProDOS disks to DOS 3.3 disks. Simply load a program from a ProDOS disk and save it to a DOS 3.3 disk with the `DSAVE` command. Because of a limitation in DOS 3.3, you cannot save programs larger than 122 sectors (30K).

While the `DLOAD` command mimics both the normal `LOAD` command and the `BLOAD` commands, `DSAVE` has two different syntaxes. The first form listed above is for use with BASIC files, similar to `SAVE`. The second form is for binary files, and is used like the `BSAVE` command. Note that in the second form, the address and length parameters must both be specified in hexadecimal (preceded with a $).

Double-Duty DOS comes in handy any time you need to move from one world to another.







