package Entidades;

public class No<T> {
    public No(int id, T dados) {
        this.id = id;
        this.dados = dados;
    }
    
    private No<T> esq;
    private No<T> dir;
    private int id;
    private T dados;
    
    
    
    public void setId(int id) {
		this.id = id;
	}

	public void setDados(T dados) {
		this.dados = dados;
	}

	public No<T> getEsq() {
        return esq;
    }
    
    public void setEsq(No<T> esq) {
        this.esq = esq;
    }
    
    public No<T> getDir() {
        return dir;
    }
    
    public void setDir(No<T> dir) {
        this.dir = dir;
    }
    
    public int getId() {
        return id;
    }
    
    public T getDados() {
        return dados;
    }
}
