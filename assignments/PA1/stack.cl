(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class List {
   content : String;
   next : List;

   content() : String { content };

   next() : List { next };

   init(s : String) : Object {
         content <- s
   };

   append(n : List) : SELF_TYPE {
      {
         next <- n;
         self;
      }
   };
};

class Stack inherits IO {
   stack : List;
   top : List;
   a2i : A2I <- new A2I;

   init() : SELF_TYPE {
      {
         stack <- new List;
         top <- stack;
         self;
      }
   };

   display() : SELF_TYPE {
      {
         out_string("start display\n");
         let l : List <- top in {
            while not l = stack loop
            {
               out_string(l.content());
               out_string("\n");
               l <- l.next();
            }
            pool;
         };
         out_string("end display\n");
         
         self;
      }
   };

   push(s: String) : Object {
      {
         let n : List <- (new List) in {
            n.init(s);
            -- out_string("push:");
            -- out_string(n.content());
            -- out_string("\n");
            top <- n.append(top);
         };
      }
   };

   pop() : String {
      let s : String in {
         if not top = stack then {
            s <- top.content();
            top <- top.next();
         } else
            out_string("no content\n") 
         fi;
         s;
      }
   };

   evaluate() : Object {
      {
         let command : String <- top.content() in {
            if command = "+" then {
               pop();
               let op1 : String <- pop(), op2 : String <- pop() in {
                  let int1 : Int <- a2i.a2i(op1), int2 : Int <- a2i.a2i(op2), result : Int in {
                     result <- int1 + int2;
                     push(a2i.i2a(result));
                  };
               };
            } else if command = "s" then {
               pop();
               let op1 : String <- pop(), op2 : String <- pop() in {
                  push(op1);
                  push(op2);
               };
            } else
               out_string("evaluate: noting to do\n")
            fi fi;
         };

         self;
      }
   };
};

class Main inherits IO {
   command : String;
   stop : Bool <- false;

   main() : Object {
      {
         let s : Stack <- (new Stack) in {
            s.init();
            while not stop loop {
               out_string(">");
               command <- in_string();
               out_string(command);
               out_string("\n");
               
               if command = "+" then
                  s.push(command)
               else if command = "s" then
                  s.push(command)
               else if command = "e" then
                  s.evaluate()
               else if command = "d" then
                  s.display()
               else if command = "x" then
                  stop <- true
               else
                  s.push(command)
               fi fi fi fi fi;
            }
            pool;
         };
      }
   };
};
