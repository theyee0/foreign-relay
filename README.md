# foreign-relay
A game where you play the role of a outpost intercepting noisy signals and making judgement calls on limited information. You're responsible for relaying a message scrambled either by digital noise or by smudging with all of the keywords intact. Be warned, though! You'll need to manage your relationship with the senders. If you keep relaying incorrect information, they'll get angry at you.

# Usage
## Demo
For those who are unable to compile and run the program on their own systems, there is a short demo:

https://github.com/user-attachments/assets/fadfef1f-68cd-4828-b3d9-ea6694d3b571

## Building
The program requires the use of `asdf`, `quicklisp`, and `cl-charms`. All instructions assume that you have those installed already. Also, for the sake of simplicity, I'll use `sbcl` in the examples, but in theory any compliant lisp compiler should work equally well.

You can create a self-contained executable program by running the `build.lisp` script:
```
sbcl --script build.lisp
```
An executable of the name `foreign-lisp.exe` should then be produced corresponding to your operating system.

### Repl
To run the program through the repl, you can use the following commands in sbcl:
```
(ql:quickload "cl-charms")
(require 'asdf)
(load "foreign-relay.asd")
(asdf:load-system "foreign-relay")
```
Then, you can run the program as `(foreign-relay:main)`.

## Running
After running the executable file or the `main` procedure through the repl, you should have a ncurses with panes for "Info", "Letter", "Input", and "Entry".

The info pane will display information about what is currently happening and in general the state of the game. Also at the start, it will have a short blurb about the program, so pay attention!

The letter pane will show the scrambled communication from the sender. The parts of the message will be scrambled, but it will be sectioned. You are responsible for decoding the `[BODY]` section, though the other parts may provide helpful context.

The input pane will echo the commands you've typed into the entry pane.

The entry pane is the main way you interact with the program. You can enter single lines of input, and just hit enter to push that to the input pane.

### Playing the Game
Initially, all civilizations will have some mixed opinion of you, and they'll send you their scrambled messages to pass on to someone else. Each decoding comes in a turn, which consists of 5 steps:

1. Accept the message
   - You will have to hit enter when you're ready for the new letter to be loaded
2. Type whatever notes you need
   - Anything not going into the final transmission will be flagged with `[NOT IN MESSAGE]:`
3. Begin your transmission by entering `<start>`
4. Type the keywords, separated by spaces or on different lines doesn't matter
5. End your transmission by entering `<end>`

After any turn, you'll see how successful you were in correctly deciphering what the original message said, and whether or not the sender was satisfied with the quality.

If you want to exit, you can either type Ctrl-C, or you can enter `<quit>` rather than accepting the message at step 1.

# Libraries Used
`cl-charms` was used to provide ncurses support. Its license has been shared here:
```
Copyright (c) 2014 Mark Fedurin <hitecnologys@gmail.com>
Copyright (c) 2010 Abhishek Reddy <http://abhishek.geek.nz>

cl-charms includes portions of code from cl-ncurses.  The copyright
notices from cl-ncurses are reproduced below.

Copyright (c) 2003 Nikodemus Siivola
Copyright (c) 2004 Marcelo Ramos <mramos@montevideo.com.uy>
Copyright (c) 2007 Jacob Gabrielson <jacobg23@pobox.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```
