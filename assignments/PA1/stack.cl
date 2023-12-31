(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackNode {
   next: StackNode;
   val: String;

   init(n: StackNode, v: String) : StackNode {
      {
         next <- n;
         val <- v;
         self;
      }
   };

   isEmptyNode() : Bool {
      val = ""
   };

   getNext() : StackNode {
      next
   };

   getVal() : String {
      val
   };

   print() : Object {
      (new IO).out_string(val)
   };
};

class Stack {
   io : IO <- new IO;
   length : Int;
   top : StackNode;

   init() : Stack {
      {
         length <- 0;
         top <- (new StackNode).init(new StackNode, "");
         self;
      }
   };

   push(val : String) : Object {
      {
         length <- length + 1;
         top <- (new StackNode).init(top, val);
      }
   };

   pop() : String {
      if length = 0 then {
         abort();
         "";
      } else {
         length <- length - 1;
         let res : String <- top.getVal() in {
            top <- top.getNext();
            res;
         };
      }
      fi
   };

   getlength() : Int {
      length
   };
 
   getTopVal() : String {
      top.getVal()
   };

   printStack() : Object {
      let temp : StackNode <- top in {
         while temp.isEmptyNode() = false loop {
            temp.print();
            io.out_string("\n");
            temp <- temp.getNext();
         } 
         pool;
      }
   };
};

class Main inherits A2I {
   
   io: IO <- new IO;
   stack: Stack <- (new Stack).init();

   doOp() : Object {
      let topVal : String <- stack.getTopVal() in {
         if topVal = "+" then {
            if stack.getlength() < 3 then {
               io.out_string("Add op oeeds at least two integers.");
               abort();
            } else {
               stack.pop();
               let val1 : Int <- a2i(stack.pop()), val2 : Int <- a2i(stack.pop()) in {
                  topVal <- i2a(val1 + val2);
                  stack.push(topVal);
               };
            }
            fi;
         } else
            if topVal = "s" then {
               if stack.getlength() < 3 then {
                  io.out_string("Swap op oeeds at least two integers.");
                  abort();
               } else {
                  stack.pop();
                  let val1 : String <- stack.pop(), val2 : String <- stack.pop() in {
                     stack.push(val1);
                     stack.push(val2);
                  };
               } fi;
            } else
               ""
            fi
         fi;
      }
   };

   execute(input: String) : Object {
      if input = "x" then {
         -- No break in cool.
         io.out_string("Stop the Stack!\n");
         abort();
      } else 
         if input = "d" then {
            stack.printStack();
         } else
            if input = "e" then {
               doOp();
            } else
               if input = "+" then {
                  stack.push(input);
               } else
                  if input = "s" then {
                     stack.push(input);
                  } else
                     -- Exclude the invalid input.
                     if input = "" then {
                        abort();
                     } else
                        stack.push(i2a(a2i(input)))
                     fi
                  fi
               fi
            fi
         fi
      fi
   };  

   main() : Object {
      let input: String in {
         while true loop {
            io.out_string(">");
            input <- io.in_string();
            execute(input);
         }
         pool;
      }
   };

};
