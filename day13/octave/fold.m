function points = fold(fold_axis, fold_pos, points)
  len = size(points, 1);
  fold_poss = fold_pos(ones(len, 1, 'int32'));
  dists = abs(points(:, fold_axis) - fold_poss);
  points(:, fold_axis) = fold_poss - dists;
endfunction

## Local Variables:
## mode: octave
## End:
