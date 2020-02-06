rel Child(x: String)
rel Boy(x: String)
rel Girl(x: String)

@test
def test01(): #{ Child, Boy, Girl } = solve #{
    Child("Jane"). 
    Child("Jim"). 
    Child("John"). 

    Boy("Jim").
    Boy("John").

    Girl(x) :- Child(x), not Boy(x).
}