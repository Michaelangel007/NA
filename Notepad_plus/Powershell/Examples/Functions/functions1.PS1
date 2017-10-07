<#DEMO: user created functions

#>

Function Get-OperatingSystemVersion
{
(Get-WmiObject -Class Win32_OperatingSystem).Version } #end Get-OperatingSystemVersion

"This OS is version $(Get-OperatingSystemVersion)"
