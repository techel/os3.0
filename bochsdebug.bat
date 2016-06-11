@cd "%~dp0"
@echo c > "%tmp%\bdbcg"
@bochsdbg -f bochsemu.bxrc -q -rc "%tmp%\bdbcg"
