C:\FSLexYacc\fslex --unicode TransLex.fsl
C:\FSLexYacc\fsyacc --module TransPar TransPar.fsy
fsc -r C:\FSLexYacc\FsLexYacc.Runtime.dll Absyn.fs TransPar.fs Translex.fs Parse.fs Trans.fs Program.fs -o CkOST.exe --standalone