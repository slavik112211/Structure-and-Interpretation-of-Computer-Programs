# (define (cube a) (* a a a))
# 
# (define (sum term a next b)
#   (if (> a b)
#       0
#       (+ (term a)
#          (sum term (next a) next b))))
# 
# (define (integral f a b dx)
#   (define (add-dx x) (+ x dx))
#   (* (sum f (+ a (/ dx 2)) add-dx b)
#      dx))

def integral(a, b, dx=0.001)
  current_x = a + dx/2
  integral = 0
  while(current_x <= b)
    integral  += yield(current_x) * dx
    current_x += dx
  end
  integral
end

puts integral(2, 4) {|x| x*x*x }