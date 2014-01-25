@rem ecalogic launcher script
@rem
@rem Environment:
@rem JAVACMD        - Java command to use (optional)
@rem JAVA_HOME      - location of a JDK home dir (mandatory)
@rem JAVA_OPTS      - JVM options (optional)

@echo off
setlocal

set ECALOGIC_HOME=%~dp0\..
set ERROR_CODE=0

set _JAVACMD=%JAVACMD%

if "%_JAVACMD%"=="" (
  if not "%JAVA_HOME%"=="" (
    if exist "%JAVA_HOME%\bin\java.exe" set "_JAVACMD=%JAVA_HOME%\bin\java.exe"
  )
)

if "%_JAVACMD%"=="" set _JAVACMD=java

"%_JAVACMD%" %JAVA_OPTS% "-Decalogic.home=%ECALOGIC_HOME%" -jar "%ECALOGIC_HOME%\bin\ecalogic.jar" %*
@endlocal

@%COMSPEC% /C exit %errorlevel% > nul
