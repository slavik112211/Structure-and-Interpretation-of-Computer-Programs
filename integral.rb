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

3 44039/160083

3 4995552594896434556236816987567426070874394012082019293839/31812691723763485888378604594585078862254431033114177637499

http://www.youtube.com/watch?v=sSx8yLOHSUs