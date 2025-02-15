from ArvoreRubroNegra import ArvoreRubroNegra


arvore = ArvoreRubroNegra()
opcao = 0
while opcao != 5:
     print("*************************************")
     print("Entre com a opcao:")
     print("** 1 **: Inserir valor")
     print("** 2 **: Remover valor")
     print("** 3 **: Buscar valor")
     print("** 4 **: Exibir Árvore")
     print("** 5 **: Sair do programa")
     print("*************************************")
     opcao = int(input("-> "))
     if opcao == 1:
          x = int(input(" Informe o valor -> "))
          arvore.inserir(x)
     elif opcao == 2:
          x = int(input(" Informe o valor -> "))
          arvore.remover(x)
     elif opcao == 3:
          x = int(input(" Informe o valor -> "))
          no = arvore.buscar(x)
          if no != arvore.TNULL:
               print(f"Valor {x} encontrado.")
          else:
               print(f"Valor {x} não encontrado.")
     elif opcao == 4:
          arvore.printarArvore()
     elif opcao == 5:
          break
