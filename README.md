# calculator-code-in-different-languages

Nurşize Turan
201104091
HW2 

I worked in Windows as operating system.
For Rust, Perl and ADA languages I used an online compiler called OneCompiler (https://onecompiler.com/ ) and run all the codes here. The codes can be run again with this.
But for Prolog and Scheme the online compilers on the internet always gave unexpected messages. So I did the necessary compiler installations. 
For Prolog I installed SWI-Prolog and developed the code here. 
For Scheme I installed Racket and developed the code here. 

CODE DESIGNS:

For Rust and Perl:

In the first method, I define precedence between arithmetic operators. The method after that contains actions according to operators. Here I added a check for division by zero and an error message for unknown operators. The both <is_valid> methods evaluates the expression to see if it contains valid characters such as numbers, arithmetic operators and parantheses or if the variable name is valid as a part of error handling.
Later I change the notation of the expression from infix to postfix, which means that the operator is no longer in between, it follows the corresponding operand pair. I used an algorithm I found on the internet called the Shunting-yard Algorithm for this conversion. The operators are added according to their precedence. For the parantheses I again use stack logic to match the true pair of LPAREN and RPAREN. The postfix expression is evaluated from left to right, for an operator, 2 numbers jump off the stack and the result of this calculation pushes the stack. For and, only the result remains on the stack. 
I also added error messages for different scenarios like division by zero, invalid character usage, missing operands etc.

For Ada;

Online compiler creates problem about some methods. I cannot install the GNU compiler in my computer. So I have to change the algorithm and make it simpler in a way that online compiler can handle. I try to implement a string tracker with number and operator holders and the algorithm based on string indexing. It detects number and operator seperatly but not like shunting-yard. It is simple classic string parsing. Necessary error handlings are added.

For Prolog and Scheme; 

I changed the algorithm I implemented before because I cannot try the shunting-yard algorithm implementation easily and cannot understand and fix the compiler errors of this algorithm since I am not in the online compiler. So I am trying to implement simpler algorithms that no longer use postfix conversion. Prolog takes the input normal, Scheme takes the input as "(+ 3 5)" instead of "3+5" because this is how this functional language uses the expressions.
For prolog, I first declare a database for storing the variables. Than actions of the operators are declared. This evaluation as a recursive computation is different from other languages I mentioned before. The recursion handles most of the code. At the top there is the base case and then the rest of the arithmetic came in evaluate units. Also there is one evaluate function for variable lookup. The rest is the error messages including validate methods which are helping for detecting the errors. 
In the main code, the input asked from the user in repeat. Different from online compiler, prints and loop worked as I aimed in this code. Online compiler somehow does not do the loop.
For Scheme, I again use recursion to evaluate arithmetic expressions. Evaluate function again will calculate the result. I changed it according to the syntax of Scheme, but the logic is the same with Prolog. There is a hash table for storing the variables and their names. The assignments can be made using “assign” like “->”. Also there is a variable lookup like Prolog. The error handling added after operator actions is the same as Prolog's. These two are simpler than the above two Rust and Perl because I do not use postfix conversion in Prolog and Scheme. The reason is that debugging was hard in the installed compilers, their expressions are complicated, so I gave up the shunting-yard algorithm and go with recursion since it is a different way to express stack-like calculations.
I added necessary error messages and instructions for the user to use the program easily.

RUN THE PROGRAMS:

For Rust, Perl and Ada:
-	First go to OneCompiler (https://onecompiler.com/ )
-	The input is asked from user. In online compiler, the inputs must be ented in STDIN area before running the code because other way, it does not wait for the input enterance and gave timeout. I think it has something to do with the online compiler. So, I enter the expressions first like given in below image for these 3 languages (Rust, Perl, Ada). Then press the run button.

![image](https://github.com/user-attachments/assets/c764b57e-f9d3-476c-9f0e-d1ae0eea11e1)

Example runs from Rust, Perl and Ada:

![examples](https://github.com/user-attachments/assets/2099cf4c-06ba-4c23-a9f2-8d3aaf7350fa)

-	They are designed to loop unless user wants to exit but I cannot understand the working logic of online compiler. Normally the code contains loop to ask user an expression while true but in online compiler I entered them one by one. 
I did not think more about it because the homework has no rules for how inputs should take.

For Prolog and Scheme:

I first tried them in online compilers too, but it was problematic when I give it a research. So I installed their compilers on Windows and changed my working environment. Unlike the above three languages, Prolog and Scheme can run in a loop unless the user tells it to stop. I guess this is the advantage of installing the compiler rather than using the online one.

For the Prolog language;
-	First I open the SWI-Prolog and consult the calculator.pl file.
  ![image](https://github.com/user-attachments/assets/8d539d24-a570-4bdc-8b6b-8cd53ddc3e89)

-	After consult is done, I run it with entering “main.”
  ![image](https://github.com/user-attachments/assets/b683fc83-844f-4d72-a1d5-c0805461b7b7)

-	Than I enter the expression, “<expression>.” Given in the picture below, on the line starts with Enter,

(1) The code calculates a complex expression. 
(2) Variable assignment 
(3) Variable arithmetic 
(4) Since there is no floating point support, it calculates the result thinking all integer.
(5) Undefined var. Error
(6) Syntax error
(7) The code calculates a simple expression.
(8) Division by zero error
(9) And when “exit.” is entered by the user, code will exit. 

![image](https://github.com/user-attachments/assets/2769e7bc-2408-40b6-946a-02bfcd8866dc)

For Scheme language;
-	First I open the DrRacket compiler and it wants to “#lang racket” at the beginning of the code so I added this expression before running. 
-	This language takes input as (+ 3 5), I did not change it because this is how the language normally works with numbers. After 

-	There are couple of Scheme examples.

![image](https://github.com/user-attachments/assets/85064266-8931-4b98-a2d4-bbe44ea780e0)

PS: I did not add floating point support because the paper did not ask for it. I sometimes make assumptions about input format, errors and some other details because the homework explanation was available for interpretation. 
Since both Prolog and Perl have .pl file extensions, I upload Perl as calculator_perl.pl and Prolog as calculator.pl
