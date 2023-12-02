
README file for Programming Assignment 1
========================================

Your directory should now contain the following files:

 Makefile
 README
 atoi.cl -> [cool root]/assignments/PA1/atoi.cl
 stack.cl
 stack.test -> [cool root]/assignments/PA1/stack.test

	The Makefile contains targets for compiling and running your
	program, as well as handing it in.

	The README contains this info. Part of the assignment is to
	answer the questions at the end of this README file.
	Just edit this file.

	atoi.cl is an implementation in Cool of the string to integer
	conversion function known from the C language.

	stack.cl is the skeleton file which you should fill in with
	your program.

	stack.test is a short test input to the stack machine.


        The symlinked files (see "man ln") are that way to emphasize
        that they are read-only.


Instructions
------------

	To compile and run your program, type:

	% gmake test

        Try it now -- it should work, and print "Nothing implemented"
        (among a few other things).


        To simply compile your program, type

        % gmake compile


	Instructions for turning in the assignment will be posted on the
	course web page.

	GOOD LUCK!

Questions on PA1
----------------

1. Describe your implementation of the stack machine in a single short
   paragraph.
- Use Stack like the forward sList


2. List 3 things that you like about the Cool programming language.
- GC
- The syntax looks cool.
- Object-oriented

3. List 3 things you DON'T like about Cool.
- Each function must have a return value.
- No "Return" Keyword, which is extremely inconvenient.
- Strange use of "if" and "while".

How to use semicolon legelly?
---
1. In a block, every sentence should end with a semicolon. For example: `let s : Int in {};` or `while loop {} pool;`
2. A sentence ends without semicolon can be used as return value, but there are some restricitions:
- Only "one" sentence in the "block" and it is not in a "{}".
```cool
-- Invalid
func() : Bool {
	-- two sentences in the "block", put them in one block.
	i <- i + 1;
	if i = 0 then {
		-- only one sentence in the "block", but in a "{ }"
		false;
	} else
		-- only one sentence in the block and not in a "{ }". So legal.
		true
	fi
};
-- valid
func() : Bool {
	{
		i <- i + 1;
		if i = 0 then {
			false;
		} else
			true
		fi;
	}
};

```