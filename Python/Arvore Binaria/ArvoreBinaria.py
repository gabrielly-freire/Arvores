from ArvoreBinariaNo import No

class ArvoreBinaria:

    def __init__(self):
        self.raiz = No(None, None, None)
        self.root = None
                    
    def buscar(self, chave):
        if self.root == None: #se a arvore estiver vazia
            return None 
        atual = self.root
        while atual.item != chave:
            if chave < atual.item:
                atual = atual.esquerda
            else:
                atual = atual.direita
            if atual == None:
                return None
        return atual

    def inserir(self, noInserido):
        novo = No(noInserido, None, None) #Cria um novo No
        if self.root == None: #Caso seja a raiz
            self.root = novo
        else: #caso nao seja a raiz
            atual = self.root
            while True:
                anterior = atual
                if noInserido <= atual.item: #novo No vai para esquerda
                    atual = atual.esquerda
                    if atual == None:
                        anterior.esquerda = novo
                        return
                else:#novo No vai para direita
                    atual = atual.direita
                    if atual == None:
                        anterior.direita = novo
                        return
    
    
    def noSucessor(self, apagar):
        paiDoSucessor = apagar
        sucessor = apagar
        atual = apagar.direita

        while atual != None:
            paiDoSucessor = sucessor
            sucessor = atual
            atual = atual.esquerda

        if sucessor != apagar.direita:
            paiDoSucessor.esquerda = sucessor.direita
            sucessor.direita = apagar.direita
        return sucessor
    
    def remover(self, noRemovido):
        if self.root == None: #Se a arvore estiver vazia
            return False
        atual =  self.root
        pai = self.root
        filhoEsquerdo = True

        while atual.item != noRemovido:
            pai = atual
            if noRemovido < atual.item:
                atual = atual.esquerda
                filhoEsquerdo = True
            else:
                atual = atual.direita
                filhoEsquerdo = False
            if atual == None:
                return False
            

        if atual.esquerda == None and atual.direita == None:
            if atual == self.root:
                self.root = None
            else:
                if filhoEsquerdo:
                    pai.esquerda = None
                else:
                    pai.direita = None
        
        elif atual.direita == None:
            if atual == self.root:
                self.root = atual.esquerda
            else:
                if filhoEsquerdo:
                    pai.esquerda = atual.esquerda
                else:
                    pai.direita = atual.esquerda
        
        elif atual.esquerda == None:
            if atual == self.root:
                self.root = atual.direita
            else:
                if filhoEsquerdo:
                    pai.esquerda = atual.direita
                else:
                    pai.direita = atual.direita

        else:
            sucessor = self.noSucessor(atual)
            if atual == self.root:
                self.root = sucessor
            else:
                if filhoEsquerdo:
                    pai.esquerda = sucessor
                else:
                    pai.direita = sucessor
            sucessor.esquerda = atual.esquerda

    def preOrdem(self, atual):
         if atual != None:
              print(atual.item, end=" ")
              self.preOrdem(atual.esquerda)
              self.preOrdem(atual.direita)

    def ordemSimetrica(self, atual):
        if atual != None:
            self.ordemSimetrica(atual.esquerda)
            print(atual.item, end=" ")
            self.ordemSimetrica(atual.direita)

    def posOrdem(self, atual):
         if atual != None:
              self.posOrdem(atual.esquerda)
              self.posOrdem(atual.direita)
              print(atual.item, end=" ")

    def printarArvore(self):
          print("\n Exibindo em pre-ordem: ",end="")
          self.preOrdem(self.root)
          print("\n Exibindo em ordem simetrica: ",end="")
          self.ordemSimetrica(self.root)
          print("\n Exibindo em pos-ordem: ",end="")
          self.posOrdem(self.root)
          print("\n")

