module m_rps
  use m_kinds
  implicit none
  private

  public :: shape_to_int, calculate_p1_score, calculate_p1_score_from_outcome, &
    outcome_to_int
contains
  elemental function shape_to_int(shape) result(res)
    !! Converts the character literal used for the shape into the corresponding
    !! integer score assigned to that shape (Rock=1, Paper=2, Scissors=3)
    !! The shapes are represented by character literals different for
    !! the first and second player: A == X == 1, B == Y == 2, C == Z == 3
    character(len=1), intent(in) :: shape
    integer(kind=i4) :: res

    res = merge(1, 0, shape == 'A' .or. shape == 'X') + &
          merge(2, 0, shape == 'B' .or. shape == 'Y') + &
          merge(3, 0, shape == 'C' .or. shape == 'Z')
  end function shape_to_int

  elemental function outcome_to_int(outcome) result(res)
  !! Converts the character literal used for the encoding the 
  !! round outcome into the corresponding value (Lose == X == 0, 
  !! Draw == Y == 3, Win == Z == 6). 
    character(len=1), intent(in) :: outcome
    integer(kind=i4) :: res

    res = merge(3, 0, outcome == 'Y') + &
          merge(6, 0, outcome == 'Z')
  end function outcome_to_int

  elemental function calculate_p1_score(p1, p2) result(res)
    !! Calculate the score of player 1 in a single round of
    !! rock, paper, scissors.
    !! Each symbol has a score (Rock=1, Paper=2, Scissors=3).
    !! The outcome of the round adds additional points
    !! (Win=6, Draw=3, Lose=0)

    !> The integer corresponding to the symbols played by the 2 players
    integer(kind=i4), intent(in) :: p1, p2
    integer(kind=i4) :: res

    if (p1 == p2) then
      ! Draw
      res = 3 + p1
    else if ((p1 == 1 .and. p2 == 3) .or. (p1 == 2 .and. p2 == 1) .or. &
             (p1 == 3 .and. p2 == 2)) then
      ! Player 1 wins
      res = 6 + p1
    else
      ! Player 1 loses
      res = p1
    end if
  end function calculate_p1_score

  elemental function calculate_p1_score_from_outcome(outcome, p2) result(res)
    !! Calculate the score of player 1 in a single round of
    !! rock, paper, scissors given the round outcome (Win=6, Draw=3, Lose=0)
    !! and they symbol played by player 2.
    !! Each symbol has a score (Rock=1, Paper=2, Scissors=3).

    !> The integer corresponding to the symbols played by player 2
    integer(kind=i4), intent(in) :: p2
    !> The integer corresponding to the outcome of the round
    integer(kind=i4), intent(in) :: outcome
    integer(kind=i4) :: res

    if (outcome == 3) then
      ! Draw
      res = outcome + p2
    else if (outcome == 0) then
      ! Player 1 loses
      if (p2 == 1) then
        res = 3
      else if (p2 == 2) then
        res = 1
      else if (p2 == 3) then
        res = 2
      end if
    else if (outcome == 6) then
      ! Player 1 wins
      if (p2 == 1) then
        res = outcome + 2
      else if (p2 == 2) then
        res = outcome + 3
      else if (p2 == 3) then
        res = outcome + 1
      end if
    end if
  end function calculate_p1_score_from_outcome
end module m_rps
