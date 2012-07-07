default: output.pedantic output.normal

output.pedantic:
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.o'  -exec rm {} \;
	ghc -fpedantic-bottoms -isrc -main-is Control.Monad.State.Class.Laws.Instances.main --make src/Control/Monad/State/Class/Laws/Instances \
          -o test.pedantic 
	./test.pedantic > output.pedantic

output.normal:
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.o'  -exec rm {} \;
	ghc                    -isrc -main-is Control.Monad.State.Class.Laws.Instances.main --make src/Control/Monad/State/Class/Laws/Instances \
          -o test.normal
	./test.normal > output.normal
