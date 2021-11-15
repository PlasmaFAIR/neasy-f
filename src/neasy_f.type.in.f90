  function neasyf_type_rank{n}(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension({array(n)}), intent(in) :: variable
    nf_type = neasyf_type(variable({slice(n)}))
  end function neasyf_type_rank{n}
