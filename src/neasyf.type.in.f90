  function neasyf_type_rank{n}(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension({array(n)}), intent(in) :: variable
    class(*), dimension({array(n)}), allocatable :: local
    allocate(local({slice(n)}), mold=variable)
    nf_type = neasyf_type(local({slice(n)}))
  end function neasyf_type_rank{n}
