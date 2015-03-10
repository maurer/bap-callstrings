int g(int x){
  return f();
}

int f(){
  return g(1);
}

int main(){
  return f();
}
