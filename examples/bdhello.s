        ; These are the defaults, but we specify them explicitly for clarity
        .BASE 0x8000
        .SIZE 0x8000

        ; The location of the TTY port
        .DEFINE tty, 0x0000

        ; The entry point is 0x8000
        .ORG 0x8000
Entry:  ; The Scratch register will hold our pointer.
        WIDEMOVE X, _hello
-       ; (Temporarily holding the pointer in B)
        MOVE B, X
        ; Load the next byte into the Scratch register.
        LOAD? X, B
        ; If the byte was zero, the string is terminated. Stop the loop.
        J.Z +
        ; Write the byte to the TTY.
        STORE tty, X
        ; Increment the pointer and restore it to the proper place in the temp
        ; register
        MOVE X, BINC
        ; (If we didn't store it in the temp register, it would be clobbered by
        ; this jump)
        J -
+       ; Now echo everything that is typed.
-       LOAD? X, tty
        ; The sign bit being set signifies that no input was ready. Output only
        ; if the sign bit was clear.
        STORE.NMI tty, X
        ; Loop!
        J -

_hello: .WORDS "Hello World!\nReady for some echo?\n\n", 0
