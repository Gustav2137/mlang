// infinite recursion

fn (int x) void rec:
  print x
  return rec(x+1)
end

fn () void main:
  rec(1)
end
