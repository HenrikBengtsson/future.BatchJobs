f <- future({
  Sys.sleep(3)
  42L
})

print(f)
print(isResolved(f))
y <- value(f)
print(y)
