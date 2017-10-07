<#DEMO Switch
can be used for multiple conditions. Also see ELSE-IF

!!!NOTE: Unlike QBASIC 4.5 and Visial Basic,
all cases which match will have their script blocks
executed, I think in the order which their conditions 
are listed in the switch statement.

If you do not want this behavior to occur, use
the break statement as show with case 1 below.
 

see Windows Powershell 3.0 Step by Step.PDF (p. 194)

#>


$a = 1
Switch ($a)
{
 1 { '$a = 1' ; break }
 2 { '$a = 2' }
 3 { '$a = 3' }
 Default { 'unable to determine value of $a' }
}
"Statement after switch"