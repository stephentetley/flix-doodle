rel Child(x: Str)
rel Boy(x: Str)
rel Girl(x: Str)

@test
def test01(): #{ Child, Boy, Girl } = solve #{
    Child("Jane"). 
    Child("Jim"). 
    Child("John"). 

    Boy("Jim").
    Boy("John").

    Girl(x) :- Child(x), not Boy(x).
}