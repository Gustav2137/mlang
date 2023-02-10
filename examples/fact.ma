//example of recursive function
fn (int n) int fact :
  if (n==1 or n==0) then
    return 1
  else
    return n*fact(n-1)
  end
end

fn () void main :
  int x
  input x
  print(fact(x))
end
