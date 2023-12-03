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