Set-ExecutionPolicy Unrestricted
Invoke-Command -FilePath setup.ps1
Invoke-Command -FilePath runner.ps1
Invoke-Command -FilePath cleanup.ps1

