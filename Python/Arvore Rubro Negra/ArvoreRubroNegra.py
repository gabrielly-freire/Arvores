from ArvoreRubroNegroNo import NoRB


class ArvoreRubroNegra:
     def __init__(self):
        self.TNULL = NoRB(None)
        self.TNULL.cor = "Preto"
        self.root = self.TNULL

     def rotacao_esquerda(self, no):
        y = no.direita
        no.direita = y.esquerda
        if y.esquerda != self.TNULL:
            y.esquerda.pai = no

        y.pai = no.pai
        if no.pai is None:
            self.root = y
        elif no == no.pai.esquerda:
            no.pai.esquerda = y
        else:
            no.pai.direita = y

        y.esquerda = no
        no.pai = y

     def rotacao_direita(self, no):
        y = no.esquerda
        no.esquerda = y.direita
        if y.direita != self.TNULL:
            y.direita.pai = no

        y.pai = no.pai
        if no.pai is None:
            self.root = y
        elif no == no.pai.direita:
            no.pai.direita = y
        else:
            no.pai.esquerda = y

        y.direita = no
        no.pai = y

     def corrigir_insercao(self, k):
        while k.pai and k.pai.cor == "Vermelho":
            if k.pai == k.pai.pai.esquerda:
                tio = k.pai.pai.direita
                if tio and tio.cor == "Vermelho":
                    k.pai.cor = "Preto"
                    tio.cor = "Preto"
                    k.pai.pai.cor = "Vermelho"
                    k = k.pai.pai
                else:
                    if k == k.pai.direita:
                        k = k.pai
                        self.rotacao_esquerda(k)
                    k.pai.cor = "Preto"
                    k.pai.pai.cor = "Vermelho"
                    self.rotacao_direita(k.pai.pai)
            else:
                tio = k.pai.pai.esquerda
                if tio and tio.cor == "Vermelho":
                    k.pai.cor = "Preto"
                    tio.cor = "Preto"
                    k.pai.pai.cor = "Vermelho"
                    k = k.pai.pai
                else:
                    if k == k.pai.esquerda:
                        k = k.pai
                        self.rotacao_direita(k)
                    k.pai.cor = "Preto"
                    k.pai.pai.cor = "Vermelho"
                    self.rotacao_esquerda(k.pai.pai)
        self.root.cor = "Preto"

     def corrigir_remocao(self, x):
        while x != self.root and x.cor == "Preto":
            if x == x.pai.esquerda:
                s = x.pai.direita
                if s.cor == "Vermelho":
                    s.cor = "Preto"
                    x.pai.cor = "Vermelho"
                    self.rotacao_esquerda(x.pai)
                    s = x.pai.direita
                if s.esquerda.cor == "Preto" and s.direita.cor == "Preto":
                    s.cor = "Vermelho"
                    x = x.pai
                else:
                    if s.direita.cor == "Preto":
                        s.esquerda.cor = "Preto"
                        s.cor = "Vermelho"
                        self.rotacao_direita(s)
                        s = x.pai.direita
                    s.cor = x.pai.cor
                    x.pai.cor = "Preto"
                    s.direita.cor = "Preto"
                    self.rotacao_esquerda(x.pai)
                    x = self.root
            else:
                s = x.pai.esquerda
                if s.cor == "Vermelho":
                    s.cor = "Preto"
                    x.pai.cor = "Vermelho"
                    self.rotacao_direita(x.pai)
                    s = x.pai.esquerda
                if s.direita.cor == "Preto" and s.esquerda.cor == "Preto":
                    s.cor = "Vermelho"
                    x = x.pai
                else:
                    if s.esquerda.cor == "Preto":
                        s.direita.cor = "Preto"
                        s.cor = "Vermelho"
                        self.rotacao_esquerda(s)
                        s = x.pai.esquerda
                    s.cor = x.pai.cor
                    x.pai.cor = "Preto"
                    s.esquerda.cor = "Preto"
                    self.rotacao_direita(x.pai)
                    x = self.root
        x.cor = "Preto"

     def substituir_no(self, u, v):
        if u.pai is None:
            self.root = v
        elif u == u.pai.esquerda:
            u.pai.esquerda = v
        else:
            u.pai.direita = v
        v.pai = u.pai

     def remover(self, chave):
        no = self.buscar(chave)
        if no == self.TNULL:
            return

        y = no
        y_cor_original = y.cor
        if no.esquerda == self.TNULL:
            x = no.direita
            self.substituir_no(no, no.direita)
        elif no.direita == self.TNULL:
            x = no.esquerda
            self.substituir_no(no, no.esquerda)
        else:
            y = self.minimo(no.direita)
            y_cor_original = y.cor
            x = y.direita
            if y.pai == no:
                x.pai = y
            else:
                self.substituir_no(y, y.direita)
                y.direita = no.direita
                y.direita.pai = y

            self.substituir_no(no, y)
            y.esquerda = no.esquerda
            y.esquerda.pai = y
            y.cor = no.cor

        if y_cor_original == "Preto":
            self.corrigir_remocao(x)

     
     def inserir(self, chave):
        novo_no = NoRB(chave)
        novo_no.esquerda = self.TNULL
        novo_no.direita = self.TNULL

        y = None
        x = self.root

        while x != self.TNULL:
            y = x
            if novo_no.item < x.item:
                x = x.esquerda
            else:
                x = x.direita

        novo_no.pai = y
        if y is None:
            self.root = novo_no
        elif novo_no.item < y.item:
            y.esquerda = novo_no
        else:
            y.direita = novo_no

        if novo_no.pai is None:
            novo_no.cor = "Preto"
            return

        if novo_no.pai.pai is None:
            return

        self.corrigir_insercao(novo_no)

     def buscar(self, chave):
          atual = self.root
          while atual != self.TNULL and chave != atual.item:
               if chave < atual.item:
                    atual = atual.esquerda
               else:
                    atual = atual.direita
          return atual
     
     def minimo(self, no):
          while no.esquerda != self.TNULL:
               no = no.esquerda
          return no
     
     def emNivel(self):
          if self.root == self.TNULL:
               print("Árvore está vazia.")
               return

          fila = [self.root]
          while fila:
               atual = fila.pop(0)
               print(f"{atual.item} ({atual.cor})", end=" ")

               if atual.esquerda != self.TNULL:
                    fila.append(atual.esquerda)

               if atual.direita != self.TNULL:
                    fila.append(atual.direita)

     def printarArvore(self):
          print("Árvore em nível:")
          self.emNivel()
          print()
