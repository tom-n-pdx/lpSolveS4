# RC Class Example
#
# from Advanced R site: http://adv-r.had.co.nz/OO-essentials.html
#

Account <- setRefClass("Account")
Account$new()
#> Reference class object of class "Account"

Account

#
# Create Class with slots
#
Account <- setRefClass("Account", fields = list(balance = "numeric"))

a <- Account$new(balance = 100)
a$balance
#> [1] 100

a$balance <- 200
a$balance
#> [1] 200

#
# Can provide accessor functions - see ?setRefClass
#
# ToDO: Create accessor method
#

#
# They do shollow copy - be carefull
#

b <- a
b$balance
#> [1] 200

a$balance <- 0
b$balance
#> [1] 0


#
# THat's why RefClassObjects come with a copy function
#
c <- a$copy()
c$balance
#> [1] 0

a$balance <- 100
c$balance
#> [1] 0

# List Class Methods
Account$methods()

# List Fields
Account$fields()

#
# Add Methods
#
Account <- setRefClass("Account",
                       fields = list(balance = "numeric"),
                       methods = list(
                         withdraw = function(x) {
                           balance <<- balance - x
                         },
                         deposit = function(x) {
                           balance <<- balance + x
                         }
                       )
)

a <- Account$new(balance = 100)
a$deposit(100)
a$balance


#
# Can build class from class
# This adds a prevention from going negative
]#
NoOverdraft <- setRefClass("NoOverdraft",
                           contains = "Account",
                           methods = list(
                             withdraw = function(x) {
                               if (balance < x) stop("Not enough money")
                               balance <<- balance - x
                             }
                           )
)
accountJohn <- NoOverdraft$new(balance = 100)
accountJohn$deposit(50)
accountJohn$balance
#> [1] 150

accountJohn$withdraw(200)
#> Error in accountJohn$withdraw(200): Not enough money

isS4(a)
#> TRUE

is(a, "refClass")
#> TRUE




