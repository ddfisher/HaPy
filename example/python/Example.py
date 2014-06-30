from HaPy import ExampleModule

print "3 * 7 is", ExampleModule.foo(3,7)
print "sum from 1 to 10 is", ExampleModule.bar(10)
print "3 > 5 is", ExampleModule.baz(3)

print "sum from 1 to 100 is", ExampleModule.arr_arg(range(101))
print "numbers from 1 to 10 are", ExampleModule.arr_ret(10)

print "complex array passing:", ExampleModule.arr_complex([range(3), [], range(100)])
print "string fun:", ExampleModule.string_fun("This isn't really a palindrome.")
print "char test:", ExampleModule.char_test("t")
