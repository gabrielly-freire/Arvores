package Entidades;

public class Arvore<T> {
    private No<T> raiz;


    public No<T> getRaiz() {
        return raiz;
    }

    public void setRaiz(No<T> raiz) {
        this.raiz = raiz;
    }
}
