function output = render(pts)
  maxes = max(pts) .+ 1;
  work = ' '(ones(maxes));
  incremented = transpose(pts) .+ 1;
  indexes = sub2ind(maxes, incremented(1, :), incremented(2, :));
  work(indexes) = '#';
  output = transpose(work);
endfunction

## Local Variables:
## mode: octave
## End:
