'************************************************
'* FOXAVR-S - V 1.04 - 2008-07-13               *
'* state and keying machine for ARDF beacons    *
'* Frank Sperber, DL6DBN/AA9KJ, www.dl6dbn.de   *
'* based on FOXCTRL/2-FOX51, DL6DBN 1994        *
'* ported from 8051 assembler to AVR and BASCOM *
'************************************************

'$noinit

'basic definitions regarding the used chip

$regfile = "m8def.dat"
$crystal = 3686400
$baud = 19200

Toneport Alias Portd.6
Ledport Alias Portd.1
Cwport Alias Portd.4
Txport Alias Portd.3
Prgport Alias Portd.0
Programpin Alias Pind.0

'definition of constants

Const Hr_min = 60                                           'a minute has 60 seconds and an hour 60 minutes
Const Ledflash = 5                                          'seconds between flashing of LED
Const Bitbyte = 8                                           '8 bits are 1 byte

'EEPROM contains main parameters, tables and keying code

$eeprom                                                     'blank due to chip error
  Data 0
Ccycle:                                                     'number of beacons in a cycle
  Data 0
Cactive:                                                    'active phase (1 up to ccycle)
  Data 0
Cslot:                                                      'duration of transmit slot time in sec
  Data 60
Cwait_hr:                                                   'hours of waiting to start
  Data 0
Cwait_min:                                                  'minutes of waiting to start
  Data 0
Cblank:                                                     'blank and reserved
  Data 0 , 0
Cticks:                                                     'number of ticks = dots per second
  Data 8                                                    'useful values 8 = 48; 9 = 54; 10 = 60 BpM
Ccalib:                                                     'timer compare value for calibration
  Data 57605%                                               '57600± @ 8; 51200± @ 9; 46080± @ 10 ticks
Ctone:                                                      'timer compare value for tone generation
  Data 36                                                   'Hz = 28800/ctone

'keying code table
'MO is the beginning of all beacons and 24 Ticks long
'each individual beacon will be added to MO
'MOE to MO5 are beacons according international standard
'MOT to MO0 and MOET to MO5T are for events with more than 5 beacons
'plus two keying codes MO and MO- for permanent beacons


Mo:                                                         'basic keying code MO
  Data 24 , &B11101110 , &B00111011 , &B10111000
Const Base = 16                                             'manually inserted table base address
_e:                                                         'MOE
  Data 06 , &B10000000 , &B00000000 , &B00000000 , &B00000000
_i:                                                         'MOI
  Data 08 , &B10100000 , &B00000000 , &B00000000 , &B00000000
_s:                                                         'MOS
  Data 10 , &B10101000 , &B00000000 , &B00000000 , &B00000000
_h:                                                         'MOH
  Data 12 , &B10101010 , &B00000000 , &B00000000 , &B00000000
_5:                                                         'MO5
  Data 14 , &B10101010 , &B10000000 , &B00000000 , &B00000000
_t:                                                         'MOT
  Data 08 , &B11100000 , &B00000000 , &B00000000 , &B00000000
_m:                                                         'MOM
  Data 12 , &B11101110 , &B00000000 , &B00000000 , &B00000000
_o:                                                         'MOO
  Data 16 , &B11101110 , &B11100000 , &B00000000 , &B00000000
_mm:                                                        'MO----
  Data 20 , &B11101110 , &B11101110 , &B00000000 , &B00000000
_0:                                                         'MO0
  Data 24 , &B11101110 , &B11101110 , &B11100000 , &B00000000
_et:                                                        'MOET
  Data 12 , &B10001110 , &B00000000 , &B00000000 , &B00000000
_it:                                                        'MOIT
  Data 14 , &B10100011 , &B10000000 , &B00000000 , &B00000000
_st:                                                        'MOST
  Data 16 , &B10101000 , &B11100000 , &B00000000 , &B00000000
_ht:                                                        'MOHT
  Data 18 , &B10101010 , &B00111000 , &B00000000 , &B00000000
_5t:                                                        'MO5T
  Data 20 , &B10101010 , &B10001110 , &B00000000 , &B00000000
_te:                                                        'MOTE
  Data 12 , &B11100010 , &B00000000 , &B00000000 , &B00000000
_ti:                                                        'MOTI
  Data 14 , &B11100010 , &B10000000 , &B00000000 , &B00000000
_ts:                                                        'MOTS
  Data 16 , &B11100010 , &B10100000 , &B00000000 , &B00000000
_th:                                                        'MOTH
  Data 18 , &B11100010 , &B10101000 , &B00000000 , &B00000000
_t5:                                                        'MOT5
  Data 20 , &B11100010 , &B10101010 , &B00000000 , &B00000000
Const Mobase = 100                                          'manually inserted table address
__:                                                         'MO
  Data 02 , &B00000000 , &B00000000 , &B00000000 , &B00000000
___:                                                        'MO-
  Data 11 , &B11111100 , &B00000000 , &B00000000 , &B00000000

$data

'definitions of variables

Dim Tim As Word                                             'timer overflow value
Dim Tone As Byte                                            'timer overflow for tone generation
Dim Active As Byte                                          'active minute
Dim Cycle As Byte                                           'number of beacons
Dim Wait_hr As Byte                                         'hours to wait before start
Dim Wait_min As Byte                                        'minutes to wait before start
Dim Slot As Byte                                            'seconds of a transmission slot
Dim Ticks As Byte                                           'tick counter
Dim Maxticks As Byte                                        'ticks per second
Dim Second As Byte                                          'second counter
Dim Minute As Byte                                          'minute counter
Dim Hour As Byte                                            'hour counter
Dim Istate As Byte                                          'internal state
Dim State As Byte                                           'main state
Dim Minmax As Byte                                          'minute/slot overflow
Dim Hrmax As Byte                                           'hour/cycle overflow
Dim Key(7) As Byte                                          'keying code
Dim Keycnt As Byte                                          'keying code size
Dim Keyptr As Byte                                          'keying pointer
Dim Kbit_i As Byte                                          'pointer to keying byte
Dim Kbyte_i As Byte                                         'pointer to keying bits
Dim Keycodes(110) As Eram Byte At Base                      'keying codes in EEPROM
Dim Temp As Byte                                            'temporarily used variable

'definitions of subroutines

Declare Sub Waiting
Declare Sub Txon

'define the ISR handler

On Oc1a Tic_ev

'PROGRAM START

'definition of I/O-Ports

Ucsrb.4 = 0                                                 'UART-RX off
Config Cwport = Output
Config Txport = Output
Config Prgport = Input

Reset Prgport                                               'hi-z on program pin

Set Txport                                                  'tx off
Set Cwport                                                  'keying off
'Config Toneport = Input                                     'tone off

If Programpin = 1 Then                                      'Programming selected?
  Ucsrb.4 = 1                                               'UART-RX on
  Print "FOXAVR www.dl6dbn.de"
  Echo On                                                   'yes
  Input "Bcn>" , Cycle                                      'ask for number of beacons
  If Cycle > 20 Then                                        'more than available?
    Print "ERR"                                             'yes, error message
  End If
  Input "Act>" , Active                                     'ask for current beacon number
  If Active > Cycle Then                                    'larger than number of beacons?
    Print "ERR"                                             'yes, error message
  End If
  Input "Dur>" , Slot                                       'ask for duration of a transmission phase [s]
  If Slot < 10 Then                                         'less than 10 seconds? I think, it makes no sense
    Print "ERR"                                             'yes, error message
  End If
  Input "Hrs>" , Wait_hr                                    'ask for hours to wait till start
  Input "Min>" , Wait_min                                   'ask for minutes to wait till start
  If Wait_min >= 60 Then                                    'larger than 59
    Print "ERR"                                             'yes, error message
    Wait_min = 0                                            'set to a useful default
  End If
  Writeeeprom Slot , Cslot                                  'write all inputs to eeprom
  Writeeeprom Cycle , Ccycle                                'for the next start
  Writeeeprom Active , Cactive
  Writeeeprom Wait_hr , Cwait_hr
  Writeeeprom Wait_min , Cwait_min
Else                                                        'no new serial input
  Readeeprom Slot , Cslot                                   'get all parameters from eeprom
  Readeeprom Cycle , Ccycle
  Readeeprom Active , Cactive
  Readeeprom Wait_hr , Cwait_hr
  Readeeprom Wait_min , Cwait_min
End If

'in any case serial output of all used parameters

Print Cycle ; "/" ; Active ; " " ; Slot ; "s " ; Wait_hr ; ":" ; Wait_min

Ucsrb.3 = 0                                                 'UART-TX off

Config Ledport = Output
Set Ledport                                                 'led off

'get timing constant

Readeeprom Maxticks , Cticks
Readeeprom Tim , Ccalib
Readeeprom Tone , Ctone


'get keying code from EEPROM
'as there wasn't a solution to create an index from EEPROM labels
'all base addresses of keying codes had to be added manually :-(

Readeeprom Keycnt , Mo
Readeeprom Key(1)
Readeeprom Key(2)
Readeeprom Key(3)

If Cycle < 2 Then                                           'permanent beacon?
  If Active = 0 Then                                        'yes, get correct keying code
    Temp = Mobase                                           'manual address of keying code
  Else
    Temp = Mobase + 5                                       'manual address of keying code
  End If
Else
  Decr Active                                               'match active to 0 and up
  Temp = Active * 5
End If
Incr Temp
Keycnt = Keycnt + Keycodes(temp)                            'number of keying ticks
Incr Temp
Key(4) = Keycodes(temp)
Incr Temp
Key(5) = Keycodes(temp)
Incr Temp
Key(6) = Keycodes(temp)
Incr Temp
Key(7) = Keycodes(temp)

'configure and preset timer1 in compare and clear mode for clock of state machine

Config Timer1 = Timer , Prescale = 8 , Clear Timer = 1 , Compare A = Disconnect
Compare1a = Tim

'configure and preset timer0 in compare and clear mode for tone generation

Config Timer2 = Timer , Prescale = 64 , Clear Timer = 1 , Compare A = Toggle
OCR2 = Tone

'enable the interrupt

Enable Compare1a
Enable Interrupts

'preset several variables
'all others are set to 0 by automatically added compiler routines

Minmax = Hr_min                                             'a minute has 60 seconds
Hrmax = Hr_min                                              'an hour 60 minutes

'main state machine - state=0 waiting for start, state=1 inactive
'state=2 active phase, state=3 permanent beacon

Do                                                          'main state machine
  Select Case State
    Case 0:                                                 'waiting state
      If Hour = Wait_hr And Minute = Wait_min Then          'waiting time over?
        Minute = 0                                          'yes, set values
        Hour = 0
        Minmax = Slot                                       'minmax becomes slot time
        Hrmax = Cycle                                       'hrmax becomes cycle
        If Cycle < 2 Then                                   'permanent beacon?
          State = 3                                         'yes, state = 3
          Reset Txport                                      'and tx on, one tick before keying
        Else
          State = 1                                         'no, state = 1
        End If
      Else
        Call Waiting                                        'waiting time not over
      End If
    Case 1:                                                 'inactive state
      If Minute = Active Then                               'active time reached?
        State = 2                                           'yes, change state
        Keyptr = 0
        Reset Txport                                        'and tx on, one tick before keying
      Else
        Istate = 0
        Call Waiting                                        'no, remain inactive
      End If
    Case 2:                                                 'active state
      If Minute <> Active Then                              'active time over?
        State = 1                                           'yes, change state
        Set Cwport                                          'and tx off immediately
        Config Toneport = Input                             'tone off
        Set Txport
      Else
        Call Txon                                           'no, remain active
      End If
    Case 3:                                                 'permanent state
        Call Txon                                           'always active
  End Select
  Idle                                                      'waiting for next tick
Loop

'the following ir-code is executed when the timer compare is reached
'counters for ticks, second, minutes and hours are increased

Tic_ev:
'Toggle Toneport                                             'DEBUG only
  Incr Ticks                                                'increase ticks
  If Ticks = Maxticks Then                                  '8 ticks are 1 second
    Ticks = 0
    Incr Second
    If Second = Minmax Then                                 'minute over?
      Second = 0                                            'yes, seconds start again
      Incr Minute                                           'increase minutes
      If Minute = Hrmax Then                                'hour/transmission cycle over?
        Minute = 0                                          'yes, start again
        Incr Hour                                           'increase
      End If
    End If
  End If
Return

'subroutine to serve LED while waiting for start or active phase
'Istate is used to determine whether it's in an inactive phase or start delay
'and to signal the LED has already flashed

Waiting:
  Config Toneport = Input                                   'tone off
  Temp = Second Mod Ledflash                                'ledflash time reached?
  If Temp = 0 Then
    If Istate = 0 Then                                      'already flashed?
      Reset Ledport                                         'no, then flash
      Istate = 1                                            'and set flashed
    Else
      Set Ledport                                           'yes, LED off
    End If
  Else                                                      'no flash time
    Set Ledport                                             'led off
    Istate = 0                                              'reset marker
  End If
Return

'subroutine to serve LED and output ports in a transmission phase
'tx is turned on and keyed, LED is keyed simultaniously

Txon:
  Kbyte_i = Keyptr / Bitbyte                                'calculate byte of keying code
  Incr Kbyte_i                                              'increment by 1 to fit array index
  Kbit_i = Keyptr Mod Bitbyte                               'calculate bit in byte
  Temp = Key(kbyte_i)                                       'get the byte
  Shift Temp , Left , Kbit_i                                'get the bit
  Temp = Temp And &B10000000
  If Temp = 0 Then                                          'bit 1 or 0?
    Config Toneport = Input                                 'tone off
    Set Cwport
    Set Ledport                                             'led and cw on
  Else                                                      '0
    Config Toneport = Output                                'tone on
    Reset Cwport                                            'led and cw off
    Reset Ledport
  End If
  Incr Keyptr                                               'increment key pointer
  If Keycnt = Keyptr Then                                   'key code completed?
    Keyptr = 0                                              'yes, start again
  End If
Return