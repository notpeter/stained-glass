DECLARE SUB NukeCursor ()
DECLARE SUB WaitForKey ()
DECLARE SUB WaitOne ()
DECLARE SUB MagicInput (InRow%, InCol%, InLen%, InDef$, in$)
DECLARE SUB TitlePage ()
DECLARE SUB PickOrigin (OrgRow%, OrgCol%)
DECLARE SUB PickDestination (DestRow%, DestCol%)
DECLARE SUB cursor ()
DECLARE SUB DrawBoard ()
DECLARE SUB DrawBorder ()
DECLARE SUB SetColor ()
DECLARE SUB SetMono ()
DECLARE SUB PrintInst (inst$, InColor%)
DECLARE SUB Quit ()
DECLARE SUB PrintScore ()
DECLARE SUB PrintMoves ()
DECLARE SUB Help ()
DECLARE SUB PrintPane (r%, c%)
DECLARE SUB StartOver ()
DECLARE SUB CheckMove ()
DECLARE SUB Move ()
DECLARE SUB Win ()
DECLARE SUB ClearBoard ()
DECLARE SUB RedrawBoard ()
DECLARE SUB Load ()
DECLARE SUB save ()
DECLARE SUB Rules ()
DECLARE SUB Panic ()
DECLARE SUB PrintHelp ()
DECLARE SUB FigureScore ()
DECLARE SUB CheckStuck ()
DECLARE SUB Lose ()
DECLARE SUB HotKeyRecovery (hot$)
DECLARE SUB Hint ()
DECLARE SUB NukeHelp ()
DECLARE SUB BackUp ()
DECLARE SUB PrintBackups ()
DECLARE SUB DestCursor ()
DECLARE SUB BackUpAllTheWay ()
DECLARE SUB LicenseInfo ()
DECLARE SUB UntagSource ()

' the following are all the many variables that I'm too lazy to pass back and
' forth between subprograms like a good little C-weenie

DIM SHARED InColor%
DIM SHARED ColorVal%(7), inst$, remainder%, ColorName$(7), in$
DIM SHARED m%(6, 12), t%(6, 12), Row%, Col%, RowMod%(8), ColMod%(8), Control$, MoveCounter%
DIM SHARED primary%, Secondary%, Tertiary%, StartOverFlag%
DIM SHARED OrgRow%, OrgCol%, OrgColor%, OrgClass%, OldInst$
DIM SHARED JumpRow%, JumpCol%, JumpColor%, JumpClass%, JumpValue%
DIM SHARED DestRow%, DestCol%, DestColor%, DestClass%
DIM SHARED BadFlag%, TitleMove%(16, 4)
DIM SHARED ColorFlag%, LastFileName$
DIM SHARED game%(108, 9)
DIM SHARED BackupCount%, MemFlag%, AbortMoveFlag%, DestFlag%
DIM SHARED GoodMove%(8, 2), prog$
DIM SHARED pane$(7, 3), class%(7)
DIM SHARED JumpTable%(7, 7), DestTable%(7, 7)

GOSUB init             ' initialize unchanging variables
CALL TitlePage         ' do the demo loop until user wants to play

start:
prog$ = "Main"         ' used in error trapping
CALL PrintHelp         ' print the menu sidebar on the right side of the screen
CALL DrawBoard         ' randomize and draw the board
Row% = 1               ' set cursor row and column to 1 at beginning
Col% = 1

Main:
 CALL PickOrigin(OrgRow%, OrgCol%)      ' get the source pane
  IF StartOverFlag% = 1 THEN            ' if user wants to restart, do it
    StartOverFlag% = 0
    GOTO start
  END IF
 CALL PickDestination(DestRow%, DestCol%) ' get the destination pane
  IF AbortMoveFlag% = 1 THEN              ' if user wants to move a different
    AbortMoveFlag% = 0                    ' pane, do it
    CALL UntagSource
    GOTO Main
  END IF
  IF StartOverFlag% = 1 THEN              ' if user wants to restart, do it
    StartOverFlag% = 0
    GOTO start
  END IF
 CALL CheckMove                           ' check that it's a legal move
 CALL Move                                ' do the move
  IF remainder% = 1 THEN                  ' if user is down to one pane,
   CALL Win                               ' declare a win
   GOTO start
  END IF
 CALL CheckStuck                          ' check for stuckness
  IF StartOverFlag% = 1 THEN              ' if user wants to restart, do it
    StartOverFlag% = 0
    GOTO start
  END IF
GOTO Main

init:
 CLS                                      ' clear the screen
 GOSUB CheckForColorCard                  ' see if user has CGA
 IF CGAFlag% = 1 THEN                     ' if user has CGA or better then
  CALL SetColor                           '   load color codes
 ELSE                                     ' if not,
  CALL SetMono                            '   load mono codes
 END IF
 CALL DrawBorder                          ' draw the frame
 LastFileName$ = "MYGAME"                 ' default file name
 primary% = 1                             ' color type one, red-blue-yellow
 Secondary% = 2                           ' color type two, green-violet-orange
 Tertiary% = 3
 Control$ = "HMPKGIQO86247931"            ' legal keys for cursor module
 ColorName$(0) = CHR$(32)                 ' blank space for empty space
  FOR i% = 1 TO 7                         ' read color abbreviations
   READ ColorName$(i%)
  NEXT i%
 DATA R,V,B,G,Y,O,W            
  FOR i% = 1 TO 8                         ' read row and column modifiers for
   READ RowMod%(i%), ColMod%(i%)          '   cursor module
  NEXT i%
 DATA -1,0,0,1,1,0,0,-1,-1,-1,-1,1,1,1,1,-1
 FOR i% = 1 TO 15                         ' read source, destination row/col
  FOR j% = 1 TO 4                         '    for each of the 15 moves in the
   READ TitleMove%(i%, j%)                '    animated title screen
  NEXT j%
 NEXT i%
 DATA 4,8,2,8
 DATA 2,8,4,6
 DATA 3,9,3,7
 DATA 3,7,5,7
 DATA 5,7,3,5
 DATA 4,7,2,5
 DATA 3,6,5,4
 DATA 2,5,4,5
 DATA 4,5,2,3
 DATA 4,6,2,4
 DATA 3,3,3,5
 DATA 5,4,3,4
 DATA 2,3,2,5
 DATA 3,4,3,6
 DATA 2,5,4,7

 FOR i% = 0 TO 7                          ' read pane images
  FOR j% = 1 TO 3
   READ pane$(i%, j%)
  NEXT j%
 NEXT i%
 DATA "   "
 DATA "   "
 DATA "   "
 DATA "ฺฤฟ"
 DATA "ณRณ"
 DATA "ภฤู"
 DATA "ษอป"
 DATA "บVบ"
 DATA "ศอผ"
 DATA "ฺฤฟ"
 DATA "ณBณ"
 DATA "ภฤู"
 DATA "ษอป"
 DATA "บGบ"
 DATA "ศอผ"
 DATA "ฺฤฟ"
 DATA "ณYณ"
 DATA "ภฤู"
 DATA "ษอป"
 DATA "บOบ"
 DATA "ศอผ"
 DATA "ษอป"
 DATA "บWบ"
 DATA "ศอผ"


 FOR j% = 0 TO 7                       ' Read jump table - jump pane = row,
  FOR s% = 0 TO 7                    '  source pane = col -- in other
   READ JumpTable%(j%, s%)     '  words, if a red pane (1) jumps
  NEXT s%
 NEXT j%                               '  is in row 1, column two -- 3, or
                                          '  blue.  Keep in mind that rows and
                                          '  cols start with zero.
 DATA -1,-1,-1,-1,-1,-1,-1,-1
 DATA -1,0,-1,0,-1,0,-1,-1
 DATA -1,3,0,1,-1,-1,-1,-1
 DATA -1,0,-1,0,-1,0,-1,-1
 DATA -1,-1,-1,5,0,3,-1,-1
 DATA -1,0,-1,0,-1,0,-1,-1
 DATA -1,5,-1,-1,-1,1,0,-1
 DATA -1,4,5,6,1,2,3,0



 FOR d% = 0 TO 7                             ' read destination table; same
  FOR s% = 0 TO 7                          ' scheme as jump table above.
   READ DestTable%(d%, s%)
  NEXT s%
 NEXT d%
 DATA -1,1,2,3,4,5,6,7
 DATA -1,1,-1,2,7,6,-1,-1
 DATA -1,-1,2,-1,-1,7,-1,-1
 DATA -1,2,-1,3,-1,4,7,-1
 DATA -1,7,-1,-1,4,-1,-1,-1
 DATA -1,6,7,4,-1,5,-1,-1
 DATA -1,-1,-1,7,-1,-1,6,-1
 DATA -1,-1,-1,-1,-1,-1,-1,7



 FOR i% = 0 TO 7                               ' read color class -- 0 = blank,
  READ class%(i%)                              '  1 = primary, 2 = secondary, 3 = tertiary
 NEXT i%
 DATA 0,1,2,1,2,1,2,3
RETURN
 
CheckForColorCard:
ON ERROR GOTO NoCGA                     ' try turning on CGA - if it's not
SCREEN 1                                '  there, ON ERROR will barf you out to
SCREEN 0                                '  NoCGA.
WIDTH 80
CGAFlag% = 1

NoCGA:
RESUME ExitCGA

ExitCGA:                                ' from here on in, any error (hopefully
ON ERROR GOTO TrapError                 ' disk errors during file i/o only)
GOTO NoError                            ' will drop out to here

TrapError:
                                  
  IF ERR = 71 THEN                                         ' disk door is open
    inst$ = "Close the drive door and try again, please."
    GOTO GotErr
 END IF
  IF ERR = 61 THEN                                         ' disk is full
    inst$ = "This disk is full -- try another."
    GOTO GotErr
  END IF
  IF ERR = 57 THEN                                         ' disk is bad
    inst$ = "There is something horribly wrong with this disk..."
    GOTO GotErr
  END IF

 ' if it gets to here, I've blown it and should be notified...

 inst$ = "Error in subprogram " + prog$ + " -- call (408) 296-5529 for help!"

GotErr:
 BEEP
 CALL PrintInst(inst$, 10)     ' print the error message
 CALL WaitForKey               ' wait for keypress
RESUME NEXT                    ' resume at statement after error

NoError:             
RETURN

SUB BackUp

 prog$ = "BackUp"
 IF MoveCounter% = 0 THEN            ' no fair trying to back up beyond start
   SOUND 475, .24
   GOTO ExitBackUp
 END IF
 CALL NukeCursor                     ' remove cursor
 m% = MoveCounter%
 JumpValue% = game%(m%, 0)           ' get last jump value to add back on
 Row% = game%(m%, 1)                 ' get source row of last move
 Col% = game%(m%, 2)                 ' get source col of last move
 PaneColor% = game%(m%, 3)           ' get source color of last source pane
 m%(Row%, Col%) = PaneColor%         ' put it back into board matrix
 CALL PrintPane(Row%, Col%)          ' put it back onto screen
 r% = game%(m%, 4)                   ' get jump row of last move
 c% = game%(m%, 5)                   ' get jump col of last move
 PaneColor% = game%(m%, 6)           ' get jump color of last move
 m%(r%, c%) = PaneColor%             ' put it back into board matrix
 CALL PrintPane(r%, c%)              ' put it back onto screen
 r% = game%(m%, 7)                   ' get dest row of last move
 c% = game%(m%, 8)                   ' get dest col of last move
 PaneColor% = game%(m%, 9)           ' get dest color of last move
 m%(r%, c%) = PaneColor%             ' put it back into board matrix
 CALL PrintPane(r%, c%)              ' put it back onto screen
 remainder% = remainder% + JumpValue% ' add jump value to remainder
 CALL PrintScore                     ' put it back onto screen
 MoveCounter% = MoveCounter% - 2     ' subtract one from move counter
 CALL PrintMoves                     ' put it back onto screen
 BackupCount% = BackupCount% + 1     ' increment backup count
 CALL PrintBackups                   ' put it onto screen

ExitBackUp:

END SUB

SUB BackUpAllTheWay

 prog$ = "BackUpAllTheWay"
 CALL NukeHelp                           ' remove help options
 inst$ = "Rewinding..."                  ' load instruction line
 CALL PrintInst(inst$, 15)               ' print instruction line
 FOR b% = MoveCounter% TO 1 STEP -1      ' do this until move < 1
  CALL BackUp                            ' back up one move
 NEXT b%
 BackupCount% = 0                        ' reset backup count
 CALL PrintBackups                       ' print backup count

END SUB

SUB CheckMove

 prog$ = "CheckMove"
 BadFlag% = 0                                            ' reset bad flag
 JumpRow% = (OrgRow% + DestRow%) / 2                     ' get jump row
 JumpCol% = (OrgCol% + DestCol%) / 2                     ' get jump col
 OrgColor% = m%(OrgRow%, OrgCol%)                        ' get org color
 JumpColor% = m%(JumpRow%, JumpCol%)                     ' get jump color
 DestColor% = m%(DestRow%, DestCol%)                     ' get dest color
 OrgClass% = class%(OrgColor%)                           ' get org class
 JumpClass% = class%(JumpColor%)                         ' get jump class
 DestClass% = class%(DestColor%)                         ' get dest class
 NewJump% = JumpTable%(JumpColor%, OrgColor%)          ' get jump result
 NewDest% = DestTable%(DestColor%, OrgColor%)          ' get dest result
  IF NewJump% = -1 OR NewDest% = -1 THEN BadFlag% = 1    ' if jump result or
                                                         '  dest result is -1
                                                         '  in our tables,
                                                         '  it's a bad move

ExitCheck:

END SUB

SUB CheckStuck

 prog$ = "CheckStuck"
 FOR tr% = 1 TO 6                            ' check all rows
  FOR tc% = 1 TO 12                          ' check all cols
    IF m%(tr%, tc%) = 0 THEN GOTO SkipSpace  ' if pane is empty, skip it
   OrgRow% = tr%                             ' you are checking org row
   OrgCol% = tc%                             ' you are checking org col
   FOR tmove% = 1 TO 8                       ' check all eight moves
    JumpRow% = OrgRow% + RowMod%(tmove%)     ' get jump row
    JumpCol% = OrgCol% + ColMod%(tmove%)     ' get jump col
    DestRow% = JumpRow% + RowMod%(tmove%)    ' get dest row
    DestCol% = JumpCol% + ColMod%(tmove%)    ' get dest col
     IF JumpRow% < 1 OR JumpRow% > 6 OR JumpCol% < 1 OR JumpCol% > 12 THEN
      GOTO SkipMove                          ' you are going offboard
     END IF
     IF DestRow% < 1 OR DestRow% > 6 OR DestCol% < 1 OR DestCol% > 12 THEN
      GOTO SkipMove                          ' you are going offboard
     END IF
     CALL CheckMove                          ' check the move
    IF BadFlag% = 0 THEN GOTO ExitCheckStuck ' if the move is good, get out

SkipMove:
  NEXT tmove%                                ' next move

SkipSpace:
 NEXT tc%                                    ' next col
NEXT tr%                                     ' next row

CALL Lose                                    ' you are stuck - say so

ExitCheckStuck:

END SUB

SUB ClearBoard

 prog$ = "ClearBoard"

 FOR r% = 4 TO 21               ' clear board by printing spaces
  LOCATE r%, 23                 ' over existing panes
  PRINT SPACE$(36);
 NEXT r%

END SUB

SUB cursor

 prog$ = "Cursor"
 hot$ = inst$                                   ' save inst line in hot$

MoveCursor:                          
 LOCATE 3 + (Row% * 3 - 1), 21 + (Col% * 3)
 COLOR 15, 0
 PRINT CHR$(219);                               ' print cursor character

CursorLoop:
 in$ = UCASE$(INKEY$)
  IF in$ = "" THEN GOTO CursorLoop              ' no key pressed - go back
  IF LEN(in$) = 2 OR VAL(in$) > 0 THEN          ' arrow key pressed
     in$ = RIGHT$(in$, 1)
     GOSUB Control
     GOTO MoveCursor
  END IF
  IF in$ = CHR$(13) THEN GOTO ExitCursor        ' Enter pressed
  IF DestFlag% = 1 THEN                         ' do Esc only if you are
   IF in$ = CHR$(27) THEN AbortMoveFlag% = 1    ' picking destination
   GOSUB AbortMove
   GOTO ExitCursor
  END IF
  IF DestFlag% = 0 THEN                         ' do following only on source
   IF in$ = "B" THEN CALL BackUp                ' back up
   IF in$ = "Q" THEN                            ' quit
    CALL Quit
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "P" THEN                            ' panic
    CALL Panic
    CALL RedrawBoard
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "E" THEN                            ' examples
    CALL Rules
    CALL RedrawBoard
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "L" THEN                            ' load
    CALL Load
    CALL ClearBoard
    CALL RedrawBoard
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "S" THEN                            ' save
    CALL save
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "H" THEN                            ' hint
    CALL Hint
    CALL HotKeyRecovery(hot$)
   END IF
   IF in$ = "R" THEN                            ' rewind
    CALL BackUpAllTheWay
    CALL HotKeyRecovery(hot$)
   END IF
  END IF
  IF StartOverFlag% = 1 THEN GOTO ExitCursor    ' get this from quit routine
GOTO MoveCursor

Control:
 FOR a% = 1 TO LEN(Control$)
  IF in$ = MID$(Control$, a%, 1) THEN GOTO GotControl ' found legal arrow$
 NEXT a%
RETURN

GotControl:
  IF a% > 8 THEN a% = a% - 8                          ' num lock is down
 trow% = Row% + RowMod%(a%)                           '
  IF DestFlag% = 1 THEN trow% = trow% + RowMod%(a%)
  IF trow% < 1 THEN trow% = 6
  IF trow% > 6 THEN trow% = 1
 tcol% = Col% + ColMod%(a%)
  IF DestFlag% = 1 THEN tcol% = tcol% + ColMod%(a%)
  IF tcol% < 1 THEN tcol% = 12
  IF tcol% > 12 THEN tcol% = 1
 CALL NukeCursor
 Row% = trow%
 Col% = tcol%
RETURN

AbortMove:
  IF Row% = OrgRow% AND Col% = OrgCol% THEN RETURN
 CALL NukeCursor
 Row% = OrgRow%
 Col% = OrgCol%
RETURN

ExitCursor:

END SUB

SUB DrawBoard

 prog$ = "DrawBoard"

 FOR r% = 1 TO 6               ' clear board
  FOR c% = 1 TO 12
   m%(r%, c%) = 0
  NEXT c%
 NEXT r%

 RANDOMIZE TIMER                             ' randomize on new seed
 PaneColor% = 0
 FOR i% = 1 TO 72                            ' randomize each of 72 panes
GetRnd:
  rr% = INT(RND * 6) + 1                     ' get rnd row
  rc% = INT(RND * 12) + 1                    ' get rnd col
   IF m%(rr%, rc%) <> 0 THEN GOTO GetRnd     ' if row, col occupied, try again
  PaneColor% = PaneColor% + 1                ' print a different pane each time
  IF PaneColor% > 6 THEN PaneColor% = 1      ' don't go over pane color 6
  m%(rr%, rc%) = PaneColor%                  ' stuff pane into board
  CALL PrintPane(rr%, rc%)                   ' print pane
 NEXT i%

 remainder% = 108       ' reset score
 CALL PrintScore        ' print score
 MoveCounter% = -1      ' reset move counter
 CALL PrintMoves        ' print move counter
 BackupCount% = 0       ' reset backup counter
 CALL PrintBackups      ' print backup counter

END SUB

SUB DrawBorder

 prog$ = "DrawBorder"
 CLS
 COLOR 15                    ' what this stuff does should be fairly obvious
 LOCATE 1, 1
 PRINT "Stained Glass v910116        Copyright Kent Brewster 1991 -- all rights reserved"
 LOCATE 3, 22
 PRINT "ษออออออออออออออออออออออออออออออออออออป"
 FOR i% = 4 TO 21
 LOCATE i%, 22
 PRINT "บ                                    บ"
 NEXT i%
 LOCATE 22, 22
 PRINT "ศออออออออออออออออออออออออออออออออออออผ"

END SUB

SUB FigureScore
 
 prog$ = "FigureScore"
                                 ' figure out value of panes to be removed
  
   JumpValue% = 1

   IF OrgClass% = primary% AND OrgColor% = DestColor% THEN
    JumpValue% = 2
   END IF
   IF OrgClass% = primary% THEN GOTO GotJumpValue

   JumpValue% = 2
   IF OrgClass% = Secondary% AND DestColor% = OrgColor% THEN
    JumpValue% = 4
   END IF
   IF OrgClass% = Secondary% THEN GOTO GotJumpValue

   JumpValue% = 3
    IF OrgColor% = DestColor% THEN
     JumpValue% = 6
    END IF

GotJumpValue:

  remainder% = remainder% - JumpValue%

END SUB

SUB Hint

 prog$ = "Hint"
  IF remainder% = 1 THEN GOTO ExitHint          ' end of game, no hint needed
 inst$ = "Press H again for another hint or any other key to continue."
 InColor% = 15                                  ' print hint message
 CALL PrintInst(inst$, InColor%)
 CALL NukeHelp                                  ' get rid of help options
HintLoop:                            
 FOR tr% = 1 TO 6                               ' check all rows
  FOR tc% = 1 TO 12                             ' check all cols
   IF m%(tr%, tc%) = 0 THEN GOTO SS1            ' if no pane there, skip it
   OrgRow% = tr%                                ' set OrgRow to temp row
   OrgCol% = tc%                                ' set OrgCol to temp col
   FOR tmove% = 1 TO 8                          ' do all 8 possible moves
    JumpRow% = OrgRow% + RowMod%(tmove%)        ' get jump row
    JumpCol% = OrgCol% + ColMod%(tmove%)        ' get jump col
    DestRow% = JumpRow% + RowMod%(tmove%)       ' get dest row
    DestCol% = JumpCol% + ColMod%(tmove%)       ' get dest col
     IF JumpRow% < 1 OR JumpRow% > 6 OR JumpCol% < 1 OR JumpCol% > 12 THEN GOTO SM1
     IF DestRow% < 1 OR DestRow% > 6 OR DestCol% < 1 OR DestCol% > 12 THEN GOTO SM1
                                                ' if move is off board, skip
    CALL CheckMove                              ' check it
     IF BadFlag% = 0 AND tc% <> hc% AND tr% <> hr% THEN GOTO FPM
                                                ' found a move - wait for key
SM1:                                 
  NEXT tmove%                                   ' next move
SS1:                                 
 NEXT tc%                                       ' next col   
NEXT tr%                                        ' next row
GOTO HintLoop                                   ' go back and get another

FPM:                                 
 CALL NukeCursor                                ' remove cursor from old loc
 Row% = tr%                                     ' set row for cursor
 Col% = tc%                                     ' set col for cursor
 LOCATE 3 + (Row% * 3 - 1), 21 + (Col% * 3)     ' get actual screen position
 COLOR 15, 0                                    ' set color
 PRINT CHR$(219);                               ' print cursor character
HintInLoop:                          
 in$ = UCASE$(INKEY$)                           ' wait for key
  IF in$ = "" THEN GOTO HintInLoop              ' if none, get another
  IF in$ <> "H" THEN GOTO ExitHint              ' if not H, get another
GOTO SS1

ExitHint:
 CALL PrintHelp                                 ' reprint help menu

END SUB

SUB HotKeyRecovery (hot$)

 prog$ = "HotKeyRecovery"
 
 CALL PrintInst(hot$, InColor%)         ' print old inst message you took off
 CALL PrintHelp                         ' replace help menu

END SUB

SUB Load

 prog$ = "Load"

 CALL NukeHelp                                  ' remove help menu
 InColor% = 15
 inst$ = "Enter game file to load or press <Esc> to abort."
 CALL PrintInst(inst$, InColor%)                ' print message     
 InRow% = 24                                    ' set input row
 InCol% = 36                                    ' set input col
 InLen% = 8                                     ' set input length
 InDef$ = LastFileName$                         ' set input default
 CALL MagicInput(InRow%, InCol%, InLen%, InDef$, in$)   ' do MagicInput
 in$ = UCASE$(in$)                              ' set in$ to uppercase
  IF in$ = "" THEN GOTO ExitLoad                ' if no input, quit             
 LastFileName$ = in$                            ' set default to in$
 sv$ = in$ + ".SAV"                             ' add file extension
 OPEN sv$ FOR RANDOM AS #1 LEN = 13             ' open it
  FIELD #1, 13 AS in$                           ' set field
 GET #1, 1                                      ' get first record
  r% = VAL(in$)                                 ' set r to value of first rec
   IF r% = 0 THEN GOSUB BadLoadFile             ' if r = 0 then it's a bad file
 remainder% = r%                                ' set remainder% to r
 GET #1, 2                                      ' get next record
 MoveCounter% = VAL(in$)                        ' set move counter to next rec
 GET #1, 3                                      ' get next record
 BackupCount% = VAL(in$)                        ' set backup count to next rec
 FOR r% = 1 TO 6                                ' get current picture of board
  GET #1, r% + 3
   FOR c% = 1 TO 12
    m%(r%, c%) = VAL(MID$(in$, c%, 1))
   NEXT c%
 NEXT r%
 FOR i% = 1 TO MoveCounter%                     ' get all moves that lead to
  GET #1, i% + 9                                '   current picture of board
  game%(i%, 0) = VAL(MID$(in$, 1, 1))           ' jump value
  game%(i%, 1) = VAL(MID$(in$, 2, 1))           ' source row
  game%(i%, 2) = VAL(MID$(in$, 3, 2))           ' source col
  game%(i%, 3) = VAL(MID$(in$, 5, 1))           ' source color
  game%(i%, 4) = VAL(MID$(in$, 6, 1))           ' jump row
  game%(i%, 5) = VAL(MID$(in$, 7, 2))           ' jump col
  game%(i%, 6) = VAL(MID$(in$, 9, 1))           ' jump color
  game%(i%, 7) = VAL(MID$(in$, 10, 1))          ' dest row
  game%(i%, 8) = VAL(MID$(in$, 11, 2))          ' dest col
  game%(i%, 9) = VAL(MID$(in$, 13, 1))          ' dest color
 NEXT i%
 CLOSE #1
 Row% = game%(MoveCounter%, 7)                  ' get current cursor row
 Col% = game%(MoveCounter%, 8)                  ' get current cursor col
  IF Row% = 0 OR Col% = 0 THEN                  ' set to one if either is 0
    Row% = 1
    Col% = 1
  END IF
 MoveCounter% = MoveCounter% - 1                ' reset move counter
 CALL PrintMoves                                ' print it
 CALL PrintScore                                ' print score
 CALL PrintBackups                              ' print backups
GOTO ExitLoad

BadLoadFile:
 inst$ = "Sorry -- I can't find " + sv$ + ".  Press any key to continue."
 InColor% = 15
 CALL PrintInst(inst$, InColor%)                ' print bad file message
 CLOSE #1
 KILL sv$                                       ' get rid of bad file
BadLoadLoop:
 IF INKEY$ = "" THEN GOTO BadLoadLoop           ' wait for a key

ExitLoad:
 LOCATE 24, 36                                  ' remove file name
 PRINT "        ";
 CALL PrintHelp                                 ' reprint help menu

END SUB

SUB Lose

 prog$ = "Lose"
 CALL NukeHelp                          ' remove help menu
 SOUND 475, .24                         ' thock
 inst$ = "Sorry, but you are stuck.  B)ack up, N)ew game, L)oad game, R)ewind, or Q)uit?"
 InColor% = ColorVal%(5)
 CALL PrintInst(inst$, InColor%)        ' print stuck message     

LoseLoop:
 in$ = UCASE$(INKEY$)                   ' convert in$ to upper case
  IF in$ = "" THEN GOTO LoseLoop        ' if nothing, try again
  IF in$ = "N" THEN                     ' new game
   StartOverFlag% = 1
   GOTO ExitStuck
  END IF
  IF in$ = "Q" THEN                     ' quit
   CALL Quit
   StartOverFlag% = 1
   GOTO ExitStuck
  END IF
  IF in$ = "R" THEN                     ' rewind
   CALL BackUpAllTheWay
   GOTO ExitStuck
  END IF
  IF in$ = "B" THEN                     ' back up
   CALL BackUp
   GOTO ExitStuck
  END IF
  IF in$ = "L" THEN                     ' load
   CALL Load
   CALL ClearBoard
   CALL RedrawBoard
   GOTO ExitStuck
  END IF
 SOUND 475, .24                         ' thock - bad input
GOTO LoseLoop                           ' go back and try again
                                        
ExitStuck:
 CALL PrintHelp                         ' reprint help menu

END SUB

SUB MagicInput (InRow%, InCol%, InLen%, InDef$, in$)

prog$ = "MagicInput"
 sf% = 1
MagicInput:
 CursorLoc = 0
 GOSUB PrintLimits                      ' print ">       <" around input area
 GOSUB ClearInLine                      ' clear that space
 GOSUB PrintInDef                       ' print the default string
 GOSUB PrintCursor                      ' print cursor
 GOSUB MInLoop                          ' get input
 GOSUB BuildIn                          ' convert screen characters to input
 GOSUB ClearInLine                      ' clear input space
 GOSUB PrintInput                       ' print input stuff
 GOSUB EraseLimits                      ' remove limits
GOTO ExitMagicInput                     ' get out

MInLoop:
 in$ = INKEY$
  IF in$ = "" THEN GOTO MInLoop
  IF in$ = CHR$(13) THEN RETURN                ' user hit enter - you are done
  IF in$ = CHR$(8) THEN GOSUB CursorBack       ' back space key      
  IF in$ = CHR$(3) THEN GOSUB ClearInLine      ' control - C
  IF in$ = CHR$(27) THEN                      ' Esc
   in$ = ""
   GOSUB EraseLimits
   GOTO ExitMagicInput
  END IF
 a% = ASC(in$)                                 ' convert in$ to ascii value
  IF (a% > 47 AND a% < 58) OR a% = 32 OR (a% > 64 AND a% < 91) OR (a% > 96 AND a% < 123) THEN GOSUB PrintChar
GOTO MInLoop                                   ' if ascii value is char, print

CursorBack:
 GOSUB EraseCursor                             ' destructive back space
 CursorLoc% = CursorLoc% - 1                   ' back cursor up
  IF CursorLoc% < 0 THEN CursorLoc% = InLen% - 1  ' move cursor to end if -1
 GOSUB PrintCursor                             ' print cursor
RETURN

CursorForward:
 GOSUB EraseCursor                             ' destructive frontspace
 CursorLoc% = CursorLoc% + 1
  IF CursorLoc% > InLen% - 1 THEN CursorLoc% = 0
 GOSUB PrintCursor
RETURN

PrintChar:
  IF sf% = 1 THEN                              ' on first keypress, clear line
   sf% = 0
   GOSUB ClearInLine
  END IF
 GOSUB EraseCursor                             ' erase cursor
 LOCATE InRow%, InCol% + CursorLoc%            ' print input char
 PRINT in$;
 GOSUB CursorForward                           ' move cursor forward              
 GOSUB PrintCursor
RETURN

BuildIn:                                        ' build input line from screen
 in$ = ""
 FOR i% = 0 TO InLen% - 1
  in$ = in$ + CHR$(SCREEN(InRow%, InCol% + i%))
 NEXT i%
  IF in$ = SPACE$(InLen%) THEN in$ = ""
 in$ = LTRIM$(RTRIM$(in$))                      ' remove spaces
RETURN

PrintCursor:
 LOCATE InRow%, InCol% + CursorLoc%
 COLOR 0, 7                                     ' reverse colors
 PRINT CHR$(SCREEN(InRow%, InCol% + CursorLoc%));  ' print what's there
 COLOR 7, 0                                     ' normalize colors
RETURN

EraseCursor:                                    ' erase cursor
 LOCATE InRow%, InCol% + CursorLoc%
 PRINT CHR$(SCREEN(InRow%, InCol% + CursorLoc%));
RETURN

EraseLimits:                                    ' remove > and <
 LOCATE InRow%, InCol% - 1
 PRINT " ";
 LOCATE InRow%, InCol% + InLen%
 PRINT " ";
RETURN

PrintInput:                                     ' print input string
 LOCATE InRow%, InCol%
 PRINT in$;
RETURN

ClearInLine:                                    ' clear input area
 LOCATE InRow%, InCol%
 PRINT SPACE$(InLen%);
RETURN

PrintLimits:                                    ' print limits
 LOCATE InRow%, InCol% - 1
 PRINT ">";
 LOCATE InRow%, InCol% + InLen%
 PRINT "<";
RETURN

PrintInDef:                                     ' print default string
 LOCATE InRow%, InCol%
 PRINT InDef$;
RETURN

ExitMagicInput:

END SUB

SUB Move

 prog$ = "Move"
 CALL FigureScore                               ' figure score
 CALL PrintScore                                ' print score
 CALL PrintMoves                                ' print move counter
  IF MemFlag% = 1 THEN GOTO DontRememberThisMove  ' don't add move to game
 m% = MoveCounter%                                '    during demo
 game%(m%, 0) = JumpValue%
 game%(m%, 1) = OrgRow%
 game%(m%, 2) = OrgCol%
 game%(m%, 3) = OrgColor%
 game%(m%, 4) = JumpRow%
 game%(m%, 5) = JumpCol%
 game%(m%, 6) = JumpColor%
 game%(m%, 7) = DestRow%
 game%(m%, 8) = DestCol%
 game%(m%, 9) = DestColor%

DontRememberThisMove:                 
  r% = OrgRow%                                  ' remove source pane
  c% = OrgCol%
  m%(r%, c%) = 0
  CALL PrintPane(r%, c%)
   IF JumpClass% = primary% OR JumpColor% = OrgColor% THEN
  r% = JumpRow%
  c% = JumpCol%
  m%(r%, c%) = 0                                ' remove jump pane
  CALL PrintPane(r%, c%)
  GOTO DoDestination
 END IF
 JumpColor% = JumpTable%(JumpColor%, OrgColor%)
 r% = JumpRow%
 c% = JumpCol%
 PaneColor% = JumpColor%
 m%(r%, c%) = PaneColor%
 CALL PrintPane(r%, c%)                         ' change jump pane

DoDestination:
 IF OrgColor% = DestColor% THEN GOTO ExitMove
 IF DestColor% = 0 THEN
   DestColor% = OrgColor%
   GOTO PrintDest
 END IF
 DestColor% = DestTable%(DestColor%, OrgColor%)' change dest pane

PrintDest:
   r% = DestRow%
   c% = DestCol%
   PaneColor% = DestColor%
   m%(r%, c%) = PaneColor%
   CALL PrintPane(r%, c%)                       ' print dest pane

ExitMove:

END SUB

SUB NukeCursor

 prog$ = "NukeCursor"                           ' remove cursor
 LOCATE 3 + (Row% * 3 - 1), 21 + (Col% * 3)     ' locate center of pane
 COLOR ColorVal%(m%(Row%, Col%)), 0             ' change color to pane color
 PRINT ColorName$(m%(Row%, Col%));              ' print pane letter

END SUB

SUB NukeHelp

 prog$ = "NukeHelp"
 FOR i% = 5 TO 21 STEP 2                        ' print blank lines
  LOCATE i%, 68                                 '    where help menu was
  PRINT SPACE$(12);
 NEXT i%

END SUB

SUB Panic

 prog$ = "Panic"
 CLS
PanicLoop:
 COLOR 7, 0
 INPUT "A:\>", in$                      ' print phoney disk prompt
 IF in$ = "" THEN GOTO PanicLoop        ' don't do anything on Enter alone
 IF UCASE$(in$) = "DIR" THEN            ' directory disk A if in$ = "DIR"
   SHELL "DIR A:"
   GOTO PanicLoop
 END IF
 IF UCASE$(in$) = "SG" THEN             ' exit to game if in$ = "SG"
   GOTO ExitPanic
 ELSE
  PRINT "Bad command or file name"      ' print error on anything else
 END IF
 PRINT
GOTO PanicLoop

ExitPanic:
 CALL DrawBorder                        ' redraw board on exit
 CALL PrintScore
 InColor% = 15
 CALL PrintInst(inst$, InColor%)
 MoveCounter% = MoveCounter% - 1
 CALL PrintMoves
 CALL PrintBackups
END SUB

SUB PickDestination (DestRow%, DestCol%)

 prog$ = "PickDestination"
 CALL NukeHelp                                  ' remove help menus
 inst$ = "Choose a flashing destination point and press Enter.  Press Esc to go back."
 InColor% = 15
 CALL PrintInst(inst$, InColor%)                ' print instruction line

DestLoop:
 DestFlag% = 1                                  ' for cursor routine
 CALL cursor                                    ' do cursor routine
 DestFlag% = 0                                  ' reset for source cursor
  IF AbortMoveFlag% = 1 THEN GOTO GotGoodMove   ' if Esc then abort move
 DestRow% = Row%                                ' set dest row to cursor row
 DestCol% = Col%                                ' set dest col to cursor col
 FOR tmove% = 1 TO 8                            ' check move
  IF GoodMove%(tmove%, 1) = DestRow% AND GoodMove%(tmove%, 2) = DestCol% THEN GOTO GotGoodMove
 NEXT tmove%
GOTO DestLoop                                   ' move was no good - try again

GotGoodMove:
 FOR tmove% = 1 TO 8                            ' un-flash flashing panes
  IF GoodMove%(tmove%, 1) = 0 THEN GOTO SkipReplace ' dont bother with bad move
  Row% = GoodMove%(tmove%, 1)                   ' set row to flashing row
  Col% = GoodMove%(tmove%, 2)                   ' set col to flashing col
  CALL NukeCursor                               ' remove flashing pane
SkipReplace:                          
 NEXT tmove%                                    ' next one
 
  IF AbortMoveFlag% = 1 THEN                    ' if abort move, reset row, col
   Row% = OrgRow%
   Col% = OrgCol%
   GOTO ExitPickDest
  END IF

 Row% = DestRow%                                ' reset row
 Col% = DestCol%                                ' reset col

ExitPickDest:

CALL PrintHelp                                  ' put help info back

END SUB

SUB PickOrigin (OrgRow%, OrgCol%)

prog$ = "PickOrigin"

PickStart:
 inst$ = "Choose a point of origin, using the arrow keys, and press Enter."
 InColor% = 15
 CALL PrintInst(inst$, InColor%)                ' print message             
 CALL cursor                                    ' get source location
 IF StartOverFlag% = 1 THEN GOTO ExitPickOrigin ' restart if restart requested
 OrgRow% = Row%                                 ' set source row to cursor row
 OrgCol% = Col%                                 ' set source col to cursor col
 IF m%(OrgRow%, OrgCol%) = 0 THEN               ' no fair moving empty space
  inst$ = "Please choose an occupied space.  Press any key to continue."
  InColor% = ColorVal%(5)
  CALL PrintInst(inst$, InColor%)               ' print message
  SOUND 475, .24                                ' thock
  CALL WaitForKey                               ' wait for key
  GOTO PickStart                                ' start over
 END IF
  FoundMoveFlag% = 0
   FOR tmove% = 1 TO 8                          ' find all moves this pane has
    GoodMove%(tmove%, 1) = 0                    ' reset good row
    GoodMove%(tmove%, 2) = 0                    ' reset good col
    JumpRow% = OrgRow% + RowMod%(tmove%)        ' set jump row
    JumpCol% = OrgCol% + ColMod%(tmove%)        ' set jump col
    DestRow% = JumpRow% + RowMod%(tmove%)       ' set dest row
    DestCol% = JumpCol% + ColMod%(tmove%)       ' set dest col
     IF JumpRow% < 1 OR JumpRow% > 6 OR JumpCol% < 1 OR JumpCol% > 12 THEN GOTO SM
     IF DestRow% < 1 OR DestRow% > 6 OR DestCol% < 1 OR DestCol% > 12 THEN GOTO SM
                                                ' if dest or jump is offscreen,
                                                '   puke
    CALL CheckMove                              ' check this move
     IF BadFlag% = 0 THEN
      FoundMoveFlag% = 1                        ' if move ok, set found flag
      GoodMove%(tmove%, 1) = DestRow%           ' set good row    
      GoodMove%(tmove%, 2) = DestCol%           ' set good col
      PaneColor% = m%(DestRow%, DestCol%)       ' get pane color
       IF PaneColor% > 0 THEN                   '
        LOCATE 3 + (DestRow% * 3 - 1), 21 + (DestCol% * 3)
        COLOR ColorVal%(PaneColor%) + 16        ' if pane > 0, flash it
        PRINT ColorName$(PaneColor%);
       ELSE
        LOCATE 3 + (DestRow% * 3 - 1), 21 + (DestCol% * 3)
        COLOR 31, 0                             ' if pane = 0, flash hole
        PRINT CHR$(240);
       END IF
     END IF
SM:
   NEXT tmove%                                  ' try next move
  IF FoundMoveFlag% = 1 THEN GOTO TagSource     ' skip following if found move
 inst$ = "That piece cannot make a legal move.  Press any key to continue."
 InColor% = ColorVal%(5)
 SOUND 475, .24                                 ' thock
 CALL PrintInst(inst$, InColor%)                ' print bad msg
 CALL WaitForKey                                ' wait for key
GOTO PickStart                                  ' try again

TagSource:                                      ' turn source pane white
 r% = OrgRow%
 c% = OrgCol%
 PaneColor% = m%(r%, c%)
 COLOR 15
  FOR p% = 1 TO 3
   PaneLineRow% = 3 + ((r% - 1) * 3 + p%)
   PaneCol% = 21 + (c% * 3 - 1)
   LOCATE PaneLineRow%, PaneCol%
   PRINT pane$(PaneColor%, p%);
  NEXT p%

ExitPickOrigin:

END SUB

SUB PrintBackups

 prog$ = "PrintBackups"
 COLOR 15, 0
 LOCATE 17, 6                   ' print backup count
 PRINT "Backups:"
 LOCATE 19, 8
 PRINT BackupCount%; "  ";

END SUB

SUB PrintHelp

 prog$ = "PrintHelp"
 COLOR 15, 0                            ' print help menu
 LOCATE 6, 68
 PRINT "B)ack Up"
 LOCATE 8, 68
 PRINT "P)anic"
 LOCATE 10, 68
 PRINT "E)xamples"
 LOCATE 12, 68
 PRINT "L)oad"
 LOCATE 14, 68
 PRINT "S)ave"
 LOCATE 16, 68
 PRINT "H)int"
 LOCATE 18, 68
 PRINT "R)ewind"
 LOCATE 20, 68
 PRINT "Q)uit"

END SUB

SUB PrintInst (inst$, InColor%)

 prog$ = "PrintInst"
 LOCATE 25, 1                   ' clear bottom line
 PRINT SPACE$(80);
 COLOR InColor%, 0
 center% = 40 - INT((LEN(inst$) / 2)) + 1       ' figure center location
 LOCATE 25, center%                             ' locate center
 PRINT inst$;                                   ' print instruction

END SUB

SUB PrintMoves

 prog$ = "PrintMoves"                   ' print move count
 COLOR 15, 0
 MoveCounter% = MoveCounter% + 1        ' this is a little lumpy, but it rings
 LOCATE 12, 7
 PRINT "Moves:"
 LOCATE 14, 8
 PRINT MoveCounter%; SPACE$(4)

END SUB

SUB PrintPane (r%, c%)

 prog$ = "PrintPane"
 PaneColor% = m%(r%, c%)                        ' get pane color from board
 COLOR ColorVal%(PaneColor%)                    ' set color to print
  FOR p% = 1 TO 3
   PaneLineRow% = 3 + ((r% - 1) * 3 + p%)       ' find pane line row
   PaneCol% = 21 + (c% * 3 - 1)                 ' find pane line col
   LOCATE PaneLineRow%, PaneCol%                ' go there
   PRINT pane$(PaneColor%, p%);                 ' print pane segment
  NEXT p%
 IF PaneColor% > 0 THEN SOUND 37, .1            ' click if pane is not blank

END SUB

SUB PrintScore

 prog$ = "PrintScore"                   ' print remainder
 LOCATE 6, 5
 COLOR 15, 0
 PRINT "  Panes"
 LOCATE 7, 5
 PRINT "remaining:"
 LOCATE 9, 7
 PRINT remainder%; SPACE$(4)

END SUB

SUB Quit

 prog$ = "Quit"
 CALL NukeHelp
 CALL NukeCursor
 inst$ = "Are you sure you want to quit?  (y/n)"        ' load instruction
 InColor% = 15                                          ' set color
 CALL PrintInst(inst$, InColor%)                        ' print it
QuitLoop:                                    
 in$ = INKEY$
  IF in$ = "" THEN GOTO QuitLoop                        ' if no input, go back
  IF in$ = "N" OR in$ = "n" THEN GOTO ExitQuit          ' doesn't want to quit
  IF in$ = "Y" OR in$ = "y" THEN                        ' does want to quit
   CALL StartOver                                       ' ask for restart
   CALL ClearBoard                                      ' restarting - clear
   StartOverFlag% = 1                                   '   and start over
   GOTO ExitQuit
  END IF
GOTO QuitLoop

ExitQuit:
 CALL PrintHelp

END SUB

SUB RedrawBoard

prog$ = "RedrawBoard"
 FOR r% = 1 TO 6
  FOR c% = 1 TO 12
   CALL PrintPane(r%, c%)               ' redraw all panes
  NEXT c%
 NEXT r%

END SUB

SUB Rules

 prog$ = "Rules"
 MemFlag% = 1                           ' tell game not to remember demo moves
 OldBack% = BackupCount%                ' save backup count
 Oldremainder% = remainder%             ' save remainder
 OldMoves% = MoveCounter% - 1           ' save move count
 BackupCount% = 0                       ' set backup count to zero
 CALL PrintBackups                      ' print backup count
 CALL NukeHelp                          ' remove help menus
 FOR r% = 1 TO 6
  FOR c% = 1 TO 12
   t%(r%, c%) = m%(r%, c%)              ' save game
   m%(r%, c%) = 0                       ' zero game
  NEXT c%
 NEXT r%

' I'm only going to comment out the first demo; the rest are identical

Demo1:
 GOSUB ZapBoard                         ' clear board
 FOR i% = 1 TO 6                        ' set two columns of panes
  m%(i%, 6) = i%
  m%(i%, 7) = i%
 NEXT i%
 CALL RedrawBoard                       ' draw them
 remainder% = 18                        ' set remainder
 CALL PrintScore                        ' print it
 MoveCounter% = -1                      ' set move counter
 CALL PrintMoves                        ' print it
 inst$ = "1: Any color may jump over itself to a blank space."
 InColor% = 15
 CALL PrintInst(inst$, InColor%)        ' print first example
 CALL WaitOne                           ' wait .5 seconds
 FOR i% = 1 TO 6
  OrgRow% = i%                          ' do six moves, all from col 6
  OrgCol% = 6                           '   to col 8
  DestRow% = i%                         '
  DestCol% = 8
  CALL CheckMove                        ' check each move
  CALL Move                             ' do each move
  CALL PrintMoves                       ' print each move
  CALL WaitOne                          ' wait .5 sec
 NEXT i%
 CALL WaitOne                           ' wait again
 GOSUB Again                            ' ask for repeat
  IF in$ = "Y" THEN GOTO Demo1          ' go back if yes

Demo2:
 inst$ = "2: Any color may jump over itself to itself."
 InColor% = 15
 CALL PrintInst(inst$, InColor%)
 GOSUB ZapBoard
 FOR i% = 1 TO 6
  m%(i%, 6) = i%
  m%(i%, 7) = i%
  m%(i%, 8) = i%
 NEXT i%
 CALL RedrawBoard
 remainder% = 27
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 CALL WaitOne
 FOR i% = 1 TO 6
  OrgRow% = i%
  OrgCol% = 8
  DestRow% = i%
  DestCol% = 6
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
 NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo2

Demo3:
 inst$ = "3: If a primary jumps over a secondary color, the primary is subtracted."
 CALL PrintInst(inst$, InColor%)
 GOSUB ZapBoard
 FOR i% = 1 TO 5
  m%(i%, 6) = i%
  m%(i%, 7) = i% + 1
 NEXT i%
 m%(6, 6) = 6
 m%(6, 7) = 1
 CALL RedrawBoard
 remainder% = 18
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 CALL WaitOne
 FOR i% = 1 TO 6 STEP 2
  OrgRow% = i%
  OrgCol% = 6
  DestRow% = i%
  DestCol% = 8
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
  OrgRow% = i% + 1
  OrgCol% = 7
  DestRow% = i% + 1
  DestCol% = 5
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
  NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo3

Demo4:
 GOSUB ZapBoard
 tb$ = "053131500153531003151530"
 char% = 0
 FOR r% = 1 TO 6
  FOR c% = 5 TO 8
   char% = char% + 1
   m%(r%, c%) = VAL(MID$(tb$, char%, 1))
  NEXT c%
 NEXT r%
 CALL RedrawBoard
 remainder% = 18
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 inst$ = "4: If a primary jumps to a different primary, the primaries combine."
 CALL PrintInst(inst$, InColor%)
 CALL WaitOne
 FOR i% = 1 TO 6 STEP 2
  OrgRow% = i%
  OrgCol% = 8
  DestRow% = i%
  DestCol% = 6
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
  OrgRow% = i% + 1
  OrgCol% = 5
  DestRow% = i% + 1
  DestCol% = 7
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
 NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo4

Demo5:
 GOSUB ZapBoard
 tb$ = "134512356"
 char% = 0
 FOR r% = 2 TO 4
  FOR c% = 5 TO 7
   char% = char% + 1
   m%(r%, c%) = VAL(MID$(tb$, char%, 1))
  NEXT c%
 NEXT r%
 CALL RedrawBoard
 remainder% = 12
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 inst$ = "5: If a primary jumps to a secondary, the result is a tertiary (white)."
 CALL PrintInst(inst$, InColor%)
 CALL WaitOne
 FOR i% = 2 TO 4
  OrgRow% = i%
  OrgCol% = 5
  DestRow% = i%
  DestCol% = 7
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
 NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo5

Demo6:
 GOSUB ZapBoard
 tb$ = "170370570"
 char% = 0
 FOR r% = 2 TO 4
  FOR c% = 5 TO 7
   char% = char% + 1
   m%(r%, c%) = VAL(MID$(tb$, char%, 1))
  NEXT c%
 NEXT r%
 CALL RedrawBoard
 remainder% = 12
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 inst$ = "6: If a primary jumps over a tertiary, the primary is subtracted."
 CALL PrintInst(inst$, InColor%)
 CALL WaitOne
 FOR i% = 2 TO 4
  OrgRow% = i%
  OrgCol% = 5
  DestRow% = i%
  DestCol% = 7
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
 NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo6

Demo7:
 GOSUB ZapBoard
 tb$ = "270470670"
 char% = 0
 FOR r% = 2 TO 4
  FOR c% = 5 TO 7
   char% = char% + 1
   m%(r%, c%) = VAL(MID$(tb$, char%, 1))
  NEXT c%
 NEXT r%
 CALL RedrawBoard
 remainder% = 15
 MoveCounter% = -1
 CALL PrintScore
 CALL PrintMoves
 inst$ = "7: If a secondary jumps over a tertiary, the secondary is subtracted."
 CALL PrintInst(inst$, InColor%)
 CALL WaitOne
 FOR i% = 2 TO 4
  OrgRow% = i%
  OrgCol% = 5
  DestRow% = i%
  DestCol% = 7
  CALL CheckMove
  CALL Move
  CALL PrintMoves
  CALL WaitOne
  CALL WaitOne
 NEXT i%
 CALL WaitOne
 GOSUB Again
  IF in$ = "Y" THEN GOTO Demo7
GOTO ExitRules

' demos end here

Again:
 inst$ = "Do you need to see that again? (y/n/Esc)"
 CALL PrintInst(inst$, InColor%)                    ' print message
AgainLoop:                   
 in$ = UCASE$(INKEY$)
  IF in$ = CHR$(27) THEN GOTO ExitRules             ' if Esc, quit demos
  IF in$ <> "N" AND in$ <> "Y" THEN GOTO AgainLoop  ' if not y or n, try again
RETURN

ZapBoard:                       ' clear board
 CALL ClearBoard
  FOR r% = 1 TO 6
   FOR c% = 1 TO 12
    m%(r%, c%) = 0
   NEXT c%
  NEXT r%
RETURN

ExitRules:
 FOR r% = 1 TO 6
  FOR c% = 1 TO 12
   m%(r%, c%) = t%(r%, c%)       ' put board back
  NEXT c%
 NEXT r%
 CALL ClearBoard
 remainder% = Oldremainder%
 CALL PrintScore
 MoveCounter% = OldMoves%
 CALL PrintMoves
 CALL PrintHelp
 BackupCount% = OldBack%
 CALL PrintBackups
 MemFlag% = 0                    ' tell move routine to remember future moves

END SUB

SUB save

 prog$ = "Save"
 DIM t$(9)                                      ' dimension temp strings
 CALL NukeHelp                                  ' remove help screen

StartSave:
 inst$ = "Enter game save file name or press <Esc> to abort."   ' set msg
 InColor% = 15                                  ' set msg color
 CALL PrintInst(inst$, InColor%)                ' print msg
 InRow% = 24                                    ' set input row
 InCol% = 36                                    ' set input col
 InLen% = 8                                     ' set input length
 InDef$ = LastFileName$                         ' set input default to last
 CALL MagicInput(InRow%, InCol%, InLen%, InDef$, in$)  ' get input
 in$ = UCASE$(in$)                              ' set input to upper case
 IF in$ = "" THEN GOTO ExitSave                 ' if input is blank, abort
 LastFileName$ = in$                            ' set input default to input
 sv$ = in$ + ".SAV"                             ' append file extension
 OPEN sv$ FOR RANDOM AS #1 LEN = 13             ' open file
  FIELD #1, 13 AS out$                          ' field file
 GET #1, 1                                      ' get remainder
  v% = VAL(out$)
  IF v% > 0 THEN GOSUB BadSaveFile              ' if remainder exists, warn
 LSET out$ = STR$(remainder%)                   ' output remainder
 PUT #1, 1                                      '
 LSET out$ = STR$(MoveCounter%)                 ' output move counter
 PUT #1, 2                                      '
 LSET out$ = STR$(BackupCount%)                 ' output backup counter
 PUT #1, 3
 FOR r% = 1 TO 6                                '
  t$ = ""                                       '
   FOR c% = 1 TO 12                             '
    z$ = LTRIM$(RTRIM$(STR$(m%(r%, c%))))       '
    t$ = t$ + z$                                ' save picture of board
   NEXT c%                                      '
  LSET out$ = t$                                '
  PUT #1, r% + 3                                '
 NEXT r%                                        '
 FOR i% = 1 TO MoveCounter%                     ' save each move
  FOR j% = 0 TO 9                               ' save each game variable
   t$(j%) = LTRIM$(RTRIM$(STR$(game%(i%, j%)))) ' make into string
  NEXT j%
  IF LEN(t$(2)) < 2 THEN t$(2) = t$(2) + " "    ' pad if needed
  IF LEN(t$(5)) < 2 THEN t$(5) = t$(5) + " "    '
  IF LEN(t$(8)) < 2 THEN t$(8) = t$(8) + " "    '
  z$ = ""
  FOR j% = 0 TO 9                               ' concatenate into one string
   z$ = z$ + t$(j%)
  NEXT j%
  LSET out$ = z$
  PUT #1, i% + 9                                ' output it into file
 NEXT i%                                        ' next move
 CLOSE #1                                       ' close file
GOTO ExitSave

BadSaveFile:
 inst$ = sv$ + " already exists.  OK to overwrite it? (y/n)"    ' set msg
 InColor% = 15                                  ' set msg color
 CALL PrintInst(inst$, InColor%)                ' print msg
BadSaveLoop:                         
 in$ = UCASE$(INKEY$)                           ' get key
  IF in$ = "" THEN GOTO BadSaveLoop             ' if blank, get another
  IF in$ = "Y" THEN                             ' if yes, return
    RETURN
  END IF
  IF in$ <> "N" THEN GOTO BadSaveLoop           ' if not N, get key     
 CLOSE #1                                       ' close file
GOTO StartSave                                  ' go back to start

ExitSave:

 LOCATE 24, 35                                  ' clear input spot
 PRINT "          ";
 CALL PrintHelp                                 ' put help back

END SUB

SUB SetColor

 prog$ = "SetColor"
  ColorVal%(0) = 0              ' blank           
  ColorVal%(1) = 4              ' red
  ColorVal%(2) = 13             ' violet
  ColorVal%(3) = 9              ' blue
  ColorVal%(4) = 10             ' green
  ColorVal%(5) = 14             ' yellow
  ColorVal%(6) = 12             ' orange
  ColorVal%(7) = 15
  ColorFlag% = 1                ' set color

END SUB

SUB SetMono

 prog$ = "SetMono"
 FOR i = 1 TO 7
  ColorVal%(i) = 7              ' set all colors to gray
 NEXT i
 

END SUB

SUB StartOver

 prog$ = "StartOver"
 inst$ = "Would you like to start a new game? (y/n)"    ' set msg
 InColor% = 15                                  ' set input color
 CALL PrintInst(inst$, InColor%)                ' print msg
StartOverLoop:
 in$ = UCASE$(INKEY$)                           ' get key
  IF in$ = "" THEN GOTO StartOverLoop           ' if none, go back
  IF in$ = "N" THEN                             ' if no, end game with this long message:
   CLS
   PRINT "      Stained Glass is distributed using the classical shareware model.  As"
   PRINT "usual, you are encouraged to make and give away (not sell) as many copies of"
   PRINT "the game as you wish, provided that you include the files SG.BAS, SG.EXE,"
   PRINT "SG.DOC, and KENTBEST.SAV.  You are furthermore encouraged to use whatever"
   PRINT "archiving or compression program you like, as long as you include all of the"
   PRINT "files named above."
   PRINT "        If you like Stained Glass and would like to lend your support to"
   PRINT "high-quality, non-copy-protected, user-supported software (and documentation"
   PRINT "with way too many hyphens and parentheses per sentence) we ask that you send"
   PRINT "ten US dollars to:"
   PRINT
   PRINT "        Brewster and Brewster"
   PRINT "        2152 Santa Cruz Avenue"
   PRINT "        Santa Clara, CA  95051"
   PRINT
   PRINT "        Any questions?  Please feel free to call us at (408) 296-5529, after"
   PRINT "six o'clock p.m., Pacific time, or drop us a line via E-mail at CompuServe"
   PRINT "account number 76516,3034.  While the money is VERY important to us -- it lets"
   PRINT "us keep writing this stuff, after all -- we would love to hear from you whether"
   PRINT "you are a registered user or not."
   PRINT "        P. S.  Yes, that file SG.BAS is source code.  You will need QuickBASIC"
   PRINT "version 4 or higher to do anything with it.  Please note that you are getting"
   PRINT "it for FREE rather than having to send an additional hundred bucks, as is"
   PRINT "usually the case.";
   
   END                                          ' end program
  END IF
  IF in$ <> "Y" THEN GOTO StartOverLoop         ' if not y, goto start
 inst$ = ""                                     ' blank bottom line
 CALL PrintInst(inst$, InColor%)

END SUB

SUB TitlePage

 prog$ = "TitlePage"
 inst$ = "Press the space bar to step through demo or Esc to begin the game."
 InCol% = 15                                    ' set msg; set color
 CALL PrintInst(inst$, InCol%)                  ' print msg          

TitleLoop1:                           
 GOSUB SetupTitlePage
 IF stepflag% = 0 THEN
  CALL WaitOne                                  ' wait .5 secs
  CALL WaitOne
 ELSE
  CALL WaitForKey
 END IF
  IF ColorFlag% = 0 THEN GOSUB NukeLetters      ' nuke letters if monochrome
 FOR mov% = 1 TO 15
 GOSUB DoMove                                   ' do title page move
 in$ = INKEY$                                   ' get key
  IF in$ = CHR$(27) THEN GOTO ExitTitlePage     ' if esc, quit
  IF in$ = CHR$(32) THEN                        ' if space, do step
  stepflag% = 1
  END IF
  IF stepflag% = 0 THEN
   CALL WaitOne                                 ' wait .5 secs
  ELSE
   CALL WaitForKey                              ' wait for keypress
    IF in$ = CHR$(27) THEN GOTO ExitTitlePage
  END IF
 NEXT mov%
GOTO TitleLoop1
                                                
DoMove:                                         ' actually make the move
 OrgRow% = TitleMove%(mov%, 1)                  ' get org row
 OrgCol% = TitleMove%(mov%, 2)                  ' get org col
 s$ = CHR$(SCREEN(3 + (OrgRow% * 3 - 1), 21 + (OrgCol% * 3)))  ' get org letter
 DestRow% = TitleMove%(mov%, 3)                 ' get dest row
 DestCol% = TitleMove%(mov%, 4)                 ' get dest col
 CALL CheckMove                                 ' check move
 j$ = CHR$(SCREEN(3 + (JumpRow% * 3 - 1), 21 + (JumpCol% * 3))) ' get dest letter
 CALL Move                                      ' do move
  IF ColorFlag% = 1 THEN                        ' print letter if color
   LOCATE 3 + (DestRow% * 3 - 1), 21 + (DestCol% * 3)
   COLOR 15, 0
   PRINT s$;
  END IF
  IF m%(JumpRow%, JumpCol%) > 0 AND ColorFlag% = 1 THEN
   LOCATE 3 + (JumpRow% * 3 - 1), 21 + (JumpCol% * 3)
   COLOR 15, 0                                  ' print letter if color
   PRINT j$;
  END IF
RETURN
                                                
SetupTitlePage:                      
 remainder% = 17                                ' set remainder
 CALL PrintScore                                ' print score
 CALL PrintBackups                              ' print backups
 MoveCounter% = -1                              ' set move counter
 CALL PrintMoves                                ' print move counter
 r% = 3                                         ' start at row 3
 FOR i% = 1 TO 7                                ' print 'STAINED'
  c% = i% + 2                                   ' set col
  PaneColor% = i%                               ' set color
   IF PaneColor% > 6 THEN PaneColor% = PaneColor% - 6 ' don't go over color 6
  m%(r%, c%) = PaneColor%                       ' set pane
  CALL PrintPane(r%, c%)                        ' print pane
  LOCATE 3 + (r% * 3 - 1), 21 + (c% * 3)        ' locate center of pane
  COLOR 15, 0                                   ' set color to bright white
  PRINT MID$("STAINED", i%, 1);                 ' print letter
 NEXT i%
 r% = 4                                         ' go to row 4
 FOR i% = 2 TO 6
  c% = i% + 2                                   ' start at col 3
  PaneColor% = 7 - i%                           ' get pane color
  m%(r%, c%) = PaneColor%                       ' set pane
  CALL PrintPane(r%, c%)                        ' print pane
  LOCATE 3 + (r% * 3 - 1), 21 + (c% * 3)        ' print letter
  COLOR 15, 0                                   ' set color  
  PRINT MID$("GLASS", i% - 1, 1);               ' print letter
 NEXT i%
RETURN
                                                
NukeLetters:                         
 CALL WaitOne                                   ' wait .5 sec
 CALL WaitOne
 CALL RedrawBoard                               ' redraw without letters
RETURN
                                                
ExitTitlePage:                       
                                                
CALL ClearBoard                                 ' clear board
 inst$ = ""                                     ' blank inst
 CALL PrintInst(inst$, InCol%)                  ' print inst
 COLOR 15, 0                                    ' set color
 LOCATE 12, 38
 PRINT "for";
 LOCATE 13, 36                                  ' print dedication
 PRINT "Annalisa.";
 CALL WaitOne                                   ' wait .5 secs

END SUB

SUB UntagSource

 prog$ = "UnTagSource"
 r% = OrgRow%                   ' set org row
 c% = OrgCol%                   ' set org col
 CALL PrintPane(r%, c%)         ' print pane

END SUB

SUB WaitForKey

 prog$ = "WaitForKey"

WaitLoop:
 in$ = INKEY$                           ' do nothing until key is pressed
 IF in$ = "" THEN GOTO WaitLoop         ' in$ = key

END SUB

SUB WaitOne

 prog$ = "WaitOne"

StartTime! = TIMER
 WHILE TIMER < StartTime! + .5          ' wait for .5 sec to pass
 WEND

END SUB

SUB Win

 prog$ = "Win"
 CALL NukeHelp                                  ' remove help
 inst$ = "Winner!  We've got a winner!!  Press any key to continue."    ' set msg
 InColor% = 15                                  ' set msg color
 CALL PrintInst(inst$, InColor%)                ' print msg
 CALL WaitForKey                                ' wait for key
 FOR r% = 1 TO 6
  FOR c% = 1 TO 12
   t%(r%, c%) = m%(r%, c%)                      ' save game to temp matrix
   m%(r%, c%) = r%                              ' set pane to r%
  NEXT c%
 NEXT r%
 inst$ = "Now, see if you can do it less than" + STR$(MoveCounter%) + " moves!"
 CALL PrintInst(inst$, InColor%)                ' print message

WinLoop:
 CALL RedrawBoard                               ' draw board (stripes)
 FOR r% = 1 TO 6                                ' do in each row
  FOR c% = 4 TO 9                               ' do from pane 4 to 9
   LOCATE 3 + (r% * 3 - 1), 21 + (c% * 3)       ' locate middle of each pane
   COLOR 15, 0                                  ' set color to bright white
   PRINT MID$("WINNER", c% - 3, 1);             ' print letter
  NEXT c%
 NEXT r%
  IF INKEY$ = "" THEN GOTO WinLoop              ' if no key, do it again
 FOR r% = 1 TO 6
  FOR c% = 1 TO 12
   m%(r%, c%) = t%(r%, c%)                      ' reset game matrix to temp
  NEXT c%
 NEXT r%
 CALL RedrawBoard                               ' draw it
 CALL save                                      ' save it?
 CALL PrintHelp                                 ' print help

END SUB

