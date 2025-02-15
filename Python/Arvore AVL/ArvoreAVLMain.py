from ArvoreAVL import ArvoreAVL

arvore = ArvoreAVL()
print("Arvore AVL")
opcao = 0
while opcao != 5:
    print("*************************************")
    print("Entre com a opcao:")
    print("** 1 **: Inserir valor")
    print("** 2 **: Excluir valor")
    print("** 3 **: Pesquisar se certo valor está presente")
    print("** 4 **: Exibir Arvore")
    print("** 5 **: Sair do programa")
    print("*************************************")
    opcao = int(input("-> "))
    if opcao == 1:
        x = int(input(" Informe o valor -> "))
        arvore.root = arvore.inserir(arvore.root, x)
    elif opcao == 2:
        x = int(input(" Informe o valor -> "))
        arvore.root = arvore.remover(arvore.root, x)
    elif opcao == 3:
          x = int(input(" Informe o valor -> "))
          if arvore.buscar(x) != None:
               print(" Valor Encontrado")
          else:
               print(" Valor nao encontrado!")	 
    elif opcao == 4:
        print("Árvore em ordem simetrica: ", end="")
        arvore.printarArvore()
        print()
    elif opcao == 5:
        break
