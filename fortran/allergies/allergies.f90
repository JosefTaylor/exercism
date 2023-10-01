
module allergies
  implicit none

contains

  logical function allergicTo(allergy_str, allergy_key)
    character(len=*), intent(in) :: allergy_str
    integer, intent(in) :: allergy_key

    allergicTo = index(allergicList(allergy_key), allergy_str) /= 0
  end function

  function allergicList(allergy_key)
    integer, intent(in) :: allergy_key
    character(len=100) :: allergicList
    integer :: i, j
    character(len=100), dimension(8) :: allergens = (/ &
      "eggs        ", &
      "peanuts     ", &
      "shellfish   ", &
      "strawberries", &
      "tomatoes    ", &
      "chocolate   ", &
      "pollen      ", &
      "cats        "/)
    
    j = allergy_key
    allergicList = ''

    do i = 8, 1, -1
      if (2**(i-1) <= j) then
        allergicList = trim(allergens(i)) // ' ' // trim(allergicList)
        j = mod(j, 2**(i-1))
      end if
    end do

  end function

end module
