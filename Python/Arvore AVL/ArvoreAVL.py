from ArvoreAVLNo import No

class ArvoreAVL:

    def __init__(self):
        self.root = None

    def altura(self, atual):
        if not atual:
            return 0
        return atual.altura

    def fatorBalanceamento(self, atual):
        if not atual:
            return 0
        return self.altura(atual.esquerda) - self.altura(atual.direita)

    def buscar(self, chave):
        if self.root == None:
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
    
    def rotacao_direita(self, noZ):
        noY = noZ.esquerda
        T3 = noY.direita

        noY.direita = noZ
        noZ.esquerda = T3

        noZ.altura = 1 + max(self.altura(noZ.esquerda), self.altura(noZ.direita))
        noY.altura = 1 + max(self.altura(noY.esquerda), self.altura(noY.direita))

        return noY

    def rotacao_esquerda(self, noZ):
        noY = noZ.direita
        T2 = noY.esquerda

        noY.esquerda = noZ
        noZ.direita = T2

        noZ.altura = 1 + max(self.altura(noZ.esquerda), self.altura(noZ.direita))
        noY.altura = 1 + max(self.altura(noY.esquerda), self.altura(noY.direita))

        return noY

    def inserir(self, atual, chave):
        if not atual:
            return No(chave)

        if chave < atual.item:
            atual.esquerda = self.inserir(atual.esquerda, chave)
        elif chave > atual.item:
            atual.direita = self.inserir(atual.direita, chave)
        else:
            return atual

        atual.altura = 1 + max(self.altura(atual.esquerda), self.altura(atual.direita))

        balanciamento = self.fatorBalanceamento(atual)

        if balanciamento > 1 and chave < atual.esquerda.item:
            return self.rotacao_direita(atual)

        if balanciamento < -1 and chave > atual.direita.item:
            return self.rotacao_esquerda(atual)

        if balanciamento > 1 and chave > atual.esquerda.item:
            atual.esquerda = self.rotacao_esquerda(atual.esquerda)
            return self.rotacao_direita(atual)

        if balanciamento < -1 and chave < atual.direita.item:
            atual.direita = self.rotacao_direita(atual.direita)
            return self.rotacao_esquerda(atual)

        return atual

    def menor_valor_no(self, atual):
        if atual is None or atual.esquerda is None:
            return atual
        return self.menor_valor_no(atual.esquerda)

    def remover(self, atual, chave):
        if not atual:
            return atual

        if chave < atual.item:
            atual.esquerda = self.remover(atual.esquerda, chave)
        elif chave > atual.item:
            atual.direita = self.remover(atual.direita, chave)
        else:
            if atual.esquerda is None:
                temp = atual.direita
                atual = None
                return temp
            elif atual.direita is None:
                temp = atual.esquerda
                atual = None
                return temp

            temp = self.menor_valor_no(atual.direita)
            atual.item = temp.item
            atual.direita = self.remover(atual.direita, temp.item)

        if atual is None:
            return atual

        atual.altura = 1 + max(self.altura(atual.esquerda), self.altura(atual.direita))

        balanciamento = self.fatorBalanceamento(atual)

        if balanciamento > 1 and self.fatorBalanceamento(atual.esquerda) >= 0:
            return self.rotacao_direita(atual)

        if balanciamento < -1 and self.fatorBalanceamento(atual.direita) <= 0:
            return self.rotacao_esquerda(atual)

        if balanciamento > 1 and self.fatorBalanceamento(atual.esquerda) < 0:
            atual.esquerda = self.rotacao_esquerda(atual.esquerda)
            return self.rotacao_direita(atual)

        if balanciamento < -1 and self.fatorBalanceamento(atual.direita) > 0:
            atual.direita = self.rotacao_direita(atual.direita)
            return self.rotacao_esquerda(atual)

        return atual

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






    