        ; (these values are the defaults)
        .BASE 0x8000
        .SIZE 0x8000
        .ORG 0x8000
        ;;; Globals
        .DEFINE tty, 0x00
        .DEFINE uhp, 0x01
        .DEFINE upot, 0x02
        .DEFINE mhp, 0x03
        ;;; "Registers"
        .DEFINE retval, 0x80
        .DEFINE param1, 0x81
        .DEFINE param2, 0x82
        .DEFINE param3, 0x83
Entry:
        ; init stack
        WIDEMOVE S, 0x8000
        ; print the welcome string
        WIDESTORE param1, helloString
        JL OutputString
        ; initialize the game
        STORE uhp, 5
        STORE upot, 3
        STORE mhp, 10
GameLoop:
        JL ShowStats
        JL PlayerAction
        JL EnemyAction
        LOAD? Q, uhp
        J.Z YouDied
        LOAD? Q, mhp
        J.Z YouWin
        J GameLoop

helloString:
        .WORDS "Hello, and welcome to Fight!\n", 0

ShowStats:
        WIDESTORE param1, statString1
        JL OutputString
        LOAD X, uhp
        STORE param1, X
        JL OutputSmallNumber
        WIDESTORE param1, statString2
        JL OutputString
        LOAD X, upot
        STORE param1, X
        JL OutputSmallNumber
        WIDESTORE param1, statString3
        JL OutputString
        LOAD X, mhp
        STORE param1, X
        JL OutputSmallNumber
        WIDESTORE param1, statString4
        JL OutputString
        RET

statString1:
        .WORDS "Your HP: ", 0
statString2:
        .WORDS "0/50  Potions: ", 0
statString3:
        .WORDS "/3\nEnemy HP: ", 0
statString4:
        .WORDS "0/100\n", 0

PlayerAction:
        WIDESTORE param1, playerActionPromptString
        JL OutputString
ReadPlayerAction:       
        LOAD? A, tty
        J.MI ReadPlayerAction
        MOVE B, 0x61            ; letter a
        MOVE? Q, SUB
        J.Z PlayerAttacks
        MOVE B, 0x41            ; letter A
        MOVE? Q, SUB
        J.Z PlayerAttacks
        MOVE B, 0x70            ; letter p
        MOVE? Q, SUB
        J.Z PlayerPotions
        MOVE B, 0x50            ; letter P
        MOVE? Q, SUB
        J.Z PlayerPotions
        J ReadPlayerAction

playerActionPromptString:
        .WORDS "Press 'a' to attack or 'p' to drink a potion.\n", 0

PlayerAttacks:
        LOAD A, mhp
        STORE mhp, ADEC
        WIDESTORE param1, playerAttackString
        J OutputString          ; tail return
playerAttackString:
        .WORDS "You attack, dealing 10 damage.\n", 0

PlayerPotions:
        LOAD? A, upot
        J.Z NoPotions
        STORE upot, ADEC
        LOAD A, uhp
        MOVE B, 3
        MOVE A, ADD
        MOVE B, 6
        MOVE? Q, SUB
        J.NMI WastedPotion
        STORE uhp, A
        WIDESTORE param1, drankPotionString
        J OutputString          ; tail return
drankPotionString:
        .WORDS "You drank a potion, restoring 30 HP.\n", 0

WastedPotion:
        STORE uhp, 5
        WIDESTORE param1, wastedPotionString
        J OutputString          ; tail return
wastedPotionString:
        .WORDS "You drank a potion, but you were too healthy and some was wasted!\n", 0

NoPotions:
        LOAD A, mhp
        STORE mhp, ADEC
        WIDESTORE param1, playerAttackString
        JL OutputString
        J ReadPlayerAction
noPotionString:
        .WORDS "You have no more potions to drink.\n", 0
        
EnemyAction:
        LOAD A, uhp
        STORE uhp, ADEC
        WIDESTORE param1, enemyAttackString
        J OutputString          ; tail return
enemyAttackString:
        .WORDS "The enemy attacks, dealing 10 damage.\n", 0

YouWin:
        WIDESTORE param1, youWinString
        JL OutputString
        J Exit
youWinString:
        .WORDS "You win!\n", 0
YouDied:
        WIDESTORE param1, youDiedString
        JL OutputString
        J Exit
youDiedString:
        .WORDS "You died!\n", 0

OutputString:
        LOAD X, param1
-
        MOVE B, X
        LOAD? X, B
        J.Z +
        STORE tty, X
        MOVE X, BINC
        J -
+
        RET

OutputSmallNumber:
        LOAD A, param1
        MOVE B, 10
        MOVE? Q, SUB
        J.NMI +
        ; small number, output it
        MOVE B, 0x30
        STORE tty, ADD
        RET
+       ; big number, output 10 :)
        STORE tty, 0x31
        STORE tty, 0x30
        RET

Exit:
        STORE tty, 0xFF
-
        J -
