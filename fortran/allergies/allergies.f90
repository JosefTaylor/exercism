
module allergies
  implicit none

contains

  logical function allergicTo(allergy_str, allergy_key)
    character(len=*), intent(in) :: allergy_str
    integer, intent(in) :: allergy_key
    integer :: j
    character(len=100) :: allergic_list
    character(len=100), dimension(8) :: allergens = (/ &
      "eggs        ", &
      "peanuts     ", &
      "shelfish    ", &
      "strawberries", &
      "tomatoes    ", &
      "chocolate   ", &
      "pollen      ", &
      "cats        "/)

    allergic_list = allergicList(allergy_key)

    j = 8 - findloc(allergens, VALUE=allergy_str, DIM=1)

    allergicTo = (allergic_list(j:j) == "1")
    
  end function


  function allergicList(allergy_key)
    integer, intent(in) :: allergy_key
    character(len=100) :: allergicList
    write(allergicList, fmt='(B0)') allergy_key
  end function



end module
