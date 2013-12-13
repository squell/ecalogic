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

:run

"%_JAVACMD%" %JAVA_OPTS% "-Decalogic.home=%ECALOGIC_HOME%" -cp "%ECALOGIC_HOME%\lib\sbt-launch.jar" xsbt.boot.Boot %*
rem fixme
if ERRORLEVEL 1 goto error
goto end

:error
set ERROR_CODE=1

:end

@endlocal

exit /B %ERROR_CODE%
