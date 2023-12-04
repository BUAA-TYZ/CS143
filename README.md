### PA1

How to use the semicolon legally?
---
1. In a "{}"block, every sentence should end with a semicolon. For example: `let s : Int in {};` or `while loop {} pool;`
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

### PA2

- How to solve `undefined reference to 'yywrap'`
  - yywrap() is used for dealing with multiple input sources.
  - When the input is used out, the yywrap is invoked to decide whether lexer continues to scan.
  - If it returns a non-zero value, the lexer stops.
  - Add `int yywrap() { return 1; }`

- Use `lexer assignments/PA1/stack.cl` to see the output.
  - `which lexer` should show something like 'bin/lexer'.  

- The precedence is extracted from cool-manual.

> 11.1 Precedence
The precedence of infix binary and prefix unary operations, from highest to lowest, is given by the
following table:
>
> .
>
> @
> 
> ~
>
> isvoid
> 
> \* /
>
> \+ -
>
> <= < =
>
> not
>
> <-
>
> All binary operations are left-associative, with the exception of assignment, which is right-associative,
and the three comparison operations, which do not associate
- In the rule section of flex
  - /* can't be unindented.
  - between '|', don't add extra space. `Int | Bool <- Wrong! Int|Bool <- Correct!`
  - Ref. Flex12 to see how to judge EOF. Don't use feof(fin) which is wrong.

Summary
---
- Overall, in this part, we generally see the result of the lexical analysis.
- We learn the different kinds of token. Each token is a pair of `<TOKEN_CLASS, lexeme>`. They are stored in tables, which will be used in later steps.
- We know the comment is ignored in this step.
- Some errors can already be detected by this step. For example, the unmatch of the comment signature, the invalid character, the string with an unescaped newline. 
- Behind the flex, we shouldn't forget that the implementation of lexical analysis: **Rexp =>NFA=>DFA=>Code**. (The code contains the corresponding DFA actually.