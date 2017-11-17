local Matrix ={}

function IsMatrix(obj)
  return getmetatable(obj) == Matrix;
end

-- Create a new matrix of specified size. 
-- Size must be >0 for both dimensions.
-- Init must be either a number or function(row, col) -> number. Defaults to constant 0 if not supplied. 
function Matrix.New (rows, cols, init)
  local self = {};
  self._storage = {};

  assert(math.type(rows) == "integer", "rows must be a integer");
  assert(math.type(cols) == "integer", "cols must be a integer");
  assert(rows > 0, "rows must be > 0");
  assert(cols > 0, "cols must be > 0");

  self._rows = rows;
  self._cols = cols;
  self._stride = self._rows; -- stride to get to next column.
  self._offset = 0; --table index of first element.

  init = init or 0;
  assert(type(init) == "number" or type(init) == "function", "Init must be function or number");


  if type(init) == "number" then --number cast to constant function returning that number. 
    local n = init;
    init = function(row, col) return n end;
  end


  for r = 1,self._rows,1 do
    for c = 1,self._cols,1 do
      local index = Matrix._get_index(self, r, c);
      self._storage[index] = init(r, c);
    end
  end

  setmetatable(self, Matrix);
  return self;
end

-- Create from table: each subtable of init is one row. Sub tables must be of equal size. 
function Matrix.FromTable(init)
  local self = {};
  self._storage = {};

  assert(#init > 0, "Init must not be empty");

  self._rows = #init; 
  self._stride = self._rows;
  self._offset = 0;

  for rowindex, row in ipairs(init) do
    --make sure row size is consistent.
    if self._cols == nil then
      self._cols = #row;
    else
      assert(#row == self._cols, "Rows have inconsistent sizes");
    end
    -- fill in column values.
    -- internal storage is column-major
    for colindex, colval in ipairs(row) do
      local index = Matrix._get_index(self, rowindex, colindex);
      assert(
        type(colval) == "number",
        "row "..rowindex..", col "..colindex..": Matrix entry is not numeric: "..tostring(colval)
      );
      --print("Set index " .. index .. "="..colval);
      self._storage[index] = colval;
    end
  end

  setmetatable(self, Matrix);
  return self;
end

function Matrix.Id(size)
  local mat = Matrix.New(size, size, 0);
  for i = 1,size,1 do
    mat[i][i] = 1;
  end
  return mat;
end

function Matrix.clone(self)
  local init = function(r, c)
    return self[r][c];
  end
  return Matrix.New(self:rows(), self:cols(), init);
end

-- Create a submatrix of this matrix referencing the same storage
function Matrix.submatrix(self, r1, c1, nrows, ncols)
  assert(r1 + nrows - 1 <= self:rows(), "submatrix rows extend past end of our rows");
  assert(c1 + ncols - 1 <= self:cols(), "submatrix cols extend past end of our cols"); 
  local o = {};
  o._storage = self._storage;
  o._rows = nrows;
  o._cols = ncols;
  --find the index of 1,1 in the new mat: 
  local first_elem_index = self:_get_index(r1, c1);
  o._offset = first_elem_index-1;

  -- local row_dif = self:rows() - nrows;
  -- o._stride = self._stride + row_dif;
  o._stride = self._stride;

  setmetatable(o, Matrix);
  return o;
end

--set submatrix: only top-left corner of source will be considered. 
function Matrix.set_submatrix(self, r1, c1, nrows, ncols, source)
  local m = self:submatrix(r1, c1, nrows, ncols);
  assert(m:rows() <= source:rows(), "Row size mismatch");
  assert(m:cols() <= source:cols(), "Col size mismatch");
  for c = 1,m:cols() do
    for r = 1,m:rows() do
      m[r][c] = source[r][c]
    end
  end
end

function Matrix._get_index(self, r, c)
  return (c-1) * self._stride + r + self._offset;
end

function Matrix.__add(a, b)
  assert(a:cols() == b:cols(), "Matricies have unequal col size");
  assert(a:rows() == b:rows(), "Matricies have unequal row size");

  local init = function(r, c)
    return a[r][c] + b[r][c];
  end
  return Matrix.New(a:rows(), a:cols(), init)

end

function Matrix.__sub(a, b)
  assert(a:cols() == b:cols(), "Matricies have unequal col size");
  assert(a:rows() == b:rows(), "Matricies have unequal row size");

  local init = function(r, c)
    return a[r][c] - b[r][c];
  end
  return Matrix.New(a:rows(), a:cols(), init)
end

function Matrix.__mul(a, b)
  if type(a) == "number" then -- scalar
    local init = function(r, c)
      return b[r][c] * a;
    end
    return Matrix.New(b:rows(), b:cols(), init);

  elseif type(b) == "number" then --also scalar.
    return Matrix.__mul(b, a);

  else --matrix by matrix
    assert(isMatrix(a) and isMatrix(b));
    assert(a:cols() == b:rows(), "Matrixes are of imcompatible size");
    local init = function(r, c)
      -- rth row of a dot cth col of b
      local sum = 0;

      for i = 1,a:cols(),1 do
        for j = 1,b:rows(),1 do
          sum = sum + a[r][i] * b[j][c];
        end
      end

      return sum;
    end
    return Matrix.New(a:rows(), b:cols(), init);
  end
end

-- Scalar division;
function Matrix.__div(a, b)
  assert(type(b) == "number", "Division only by number supported");
  return a * (1/b);
end

function Matrix.__unm(self)
  return -1.0 * self;
end

-- Returns the matrix in a row-major table form. 
function Matrix.as_table(self)
  local rows = {};
  for r = 1,self:rows(),1 do
    local cols = {};
    for c = 1,self:cols(),1 do
      cols[#cols+1] = self[r][c];
    end
    rows[#rows+1] = cols;
  end
  return rows;
end



function Matrix.__tostring(self)
  local t = self:as_table();
  local st = {}; --stringified table.
  local maxwidth = 0;
  for r, row in ipairs(t) do
    st[r] = {};
    for c, val in ipairs(row) do
      local s = tostring(val);
      maxwidth = maxwidth > #s and maxwidth or #s;
      st[r][c] = s;
    end
  end
  
  -- now we pad all strings to the required size. 
  local fmtstring = "%"..maxwidth.."s";

  for r = 1, #t do
    local col = st[r];
    for c = 1, #col do
      col[c] = string.format(fmtstring, col[c]);
    end
    st[r] = table.concat(col, ", ");
  end
  return table.concat(st, "\n");
end


function Matrix.__len(self)
  if self:is_row_vector() then
    return self:cols();
  elseif self:is_col_vector() then
    return self:rows();
  elseif self:is_square() then
    return self:rows();
  else
    error("Attempted to take length of non-square matrix");
  end
end

function Matrix.is_row_vector(self)
  return self:rows() == 1;
end;

function Matrix.is_col_vector(self)
  return self:cols() == 1;
end

function Matrix.is_vector(self)
  return self:is_row_vector() or self:is_col_vector();
end

function Matrix.is_square(self)
  return self:rows() == self:cols();
end

function Matrix.length(self)
  assert(self:is_vector(), "Length is only defined for vectors");
  return math.sqrt(self:dot(self));
end

function Matrix.dot(self, other)
  assert(
    (self:is_row_vector() and other:is_row_vector()) or (self:is_col_vector() and other:is_col_vector()),
    "dot only defined on two row- or column- vectors"
  );
  local sum = 0;

  if(self:is_row_vector()) then
    assert(self:cols() == other:cols(), "Vectors are not of same length!");
    for i = 1,self:cols(),1 do
      sum = sum + self[1][i] * other[1][i];
    end
  else
    assert(self:rows() == other:rows(), "Vectors are not of same length!");
    for i = 1,self:rows(),1 do
      sum = sum + self[i][1] * other[i][1];
    end
  end
  return sum;

end;



-- override [][] to get sub-element. First arg is row, second is col.
function Matrix.__index(self, key)

  if type(key) == "number" then --special case: index returns a proxy to the column. 
    assert(math.type(key) == "integer", "row dereference with non-integer: "..key);

    local row = key;
    assert(row > 0 and row <= self._rows, "Row index is out of range: "..row);

    local proxyget = function(sender, col)
      assert(math.type(col) == "integer", "column dereference with non-integer: "..col);

      assert(col > 0 and col <= self._cols, "Column index out of range: "..col);
      local index = self:_get_index(row, col);
      return self._storage[index];
    end

    local proxyset = function(sender, col, value)
      assert(math.type(col) == "integer", "column dereference with non-integer.");
      assert(col > 0 and col <= self._cols, "Column index out of range: "..col);
      assert(type(value) == "number", "Attempted to set a non-number as value in matrix: "..tostring(value));
      local index = self:_get_index(row, col);
      self._storage[index] = value;
    end

    local proxyobject = {};
    setmetatable(proxyobject, {__index = proxyget, __newindex = proxyset});
    return proxyobject;

  else -- not number, perform regular index dereference. 
    return Matrix[key];
  end
end

-- Return number of rows. 
function Matrix.rows(self)
  return self._rows;
end

-- Return number of cols.
function Matrix.cols(self)
  return self._cols;
end

-- Matrix transpose. 
function Matrix.transpose(self)
  local ret = Matrix.New(self:cols(), self:rows(), 0);
  for r = 1,self:rows(),1 do
    for c = 1,self:cols(),1 do
      ret[c][r] = self[r][c];
    end
  end
  return ret;
end

function Matrix.swap_rows(self, r1, r2)
  for i = 1,self:cols() do
    local temp = self[r1][i];
    self[r1][i] = self[r2][i];
    self[r2][i] = temp;
  end
end

function Matrix.swap_cols(self, c1, c2)
  for i = 1,self:rows() do
    local temp = self[i][c1] ;
    self[i][c1] = self[i][c2];
    self[i][c2] = temp;
  end
end


-- returns matricies L, U, P such that self = L * U * P (P: permutation matrix)
function Matrix.lu_factor(self)
  assert(self:is_square(), "LU factorization requires square matricies");
  local N = self:rows();
  if N == 1 then -- base case, we're already triangular
    local one = Matrix.New[1, 1, 0];
    return self, one, one:clone();

  elseif N == 2 then
    if self[1][2] == 0 then --we're already lower-triangluar
      local id = Matrix.Id(2);
      return self:clone(), id, id:clone();
    elseif self[1][1] == 0 then --a simple permutation will set us straight. 
      local permutation = Matrix.Id(2);
      permutation:swap_cols(1, 2);
      return self:clone() ,  Matrix.Id(2), permutation;
    else --we have to get creative. 
      local P = Matrix.Id(2);
      local A = self:clone();
      if A[1][1] == 0 then
        P:swap_cols(1, 2);
        A:swap_cols(1, 2);
      end
      local U = Matrix.New(2, 2, 0);
      U[1][1] = A[1][1];
      U[1][2] = A[1][2];
      U[2][2] = A[2][2] * (A[2][2] - A[2][1] * A[1][2] / A[1][1]);
      --now AU = L
      local Lprime = Matrix.Id(N);
      LPrime[2][1] = -self[2][1] / A[1][1];
      local L = L:inverse();
      return L, U, A;
    end

  else -- we're big, so do the doolittle
    let A = self:clone();
    for n = 1, N do


      
  end

end

function Matrix.crout_lup_lu(self)
  local N = self:rows();
  local P1 = Matrix.Id(N);
  for i = 1,N do
    if self[1][n] ~= 0 then -- nonzero in first row
      -- swap columns in p1 to here to create a permutation matrix that places non-zero in upper-left. 
      P1:swap_cols(1, i);
      break;
    end
  end
  local A1 = self * P1;
  local A2 = A:submatrix(2, 2, N-1, N-1)
  local L2, U2, P2 = A2:lu_factor(); --recursive step

  L = Matrix.New(N, N, 0);
  L:set_submatrix(1, 1, N, 1, A1); -- first column is first column of A1
  L:set_submatrix(2, 2, N-1, N-1, L2); -- lower-right corner is L2

  local U3 = Matrix.New(N, N, 0);
  U3[1][1] = 1;
  U3:set_submatrix(2, 2, N-1, N-1, U2);
  local P3 = Matrix.New(N, N, 0);
  P3[1][1] = 1;
  P3:set_submatrix(2, 2, N-1, N-1, P2);
  --NB: inverse of permutation matrix is its transpose. 
  local A3 = A1 * P3:transpose();
  local P = P3 * P1:transpose();
  --A3 is LU3, except at first row maybe. 
  --
  --check for first row of A zero
  for i = 1,N do
    if A[1][i] ~= 0 then --first row of A is non-zero
      --fuck this. 

      break;
    end
  end

  --first row of A is zero, so: 
  return L, U3, P;

end


function Matrix.det(self)
  assert(self:is_square(), "Determinant is only defined for square matricies");
  if self:rows() == 1 then
    return self[1][1];
  elseif self:rows() == 2 then
    return self[1][1] * self[2][2] - self[1][2] * self[2][1];
  else
    --TODO: implement this.
    error("Not implemented");
  end
end

function Matrix.inverse(self)
  assert(self:is_square(), "Inverse is only defined for square matricies");
  if self:rows() == 1 then
    return Matrix.New(1, 1, 1/self[1][1]);
  elseif self:rows() == 2 then
    local ret = Matrix.New(2, 2, 0);
    local det = self:det();
    assert(det ~= 0, "Matrix is non-invertible");
    ret[1][1] = self[2][2];
    ret[2][2] = self[1][1];
    ret[1][2] = - self[1][2];
    ret[2][1] = - self[2][1];
    return (1/det) * ret;
  else
    error("Not implemented for size > 2");

  end


end




print "==Matrix.New=="
m = Matrix.New(3, 3, 3.14);
print("rows="..m._rows);
print("cols="..m._cols);
print("Storage: ");
for k, v in ipairs(m._storage) do
  print(""..k..": "..tostring(v));
end

print(m[2][1])

print(tostring(m));

print "==submatrix=="
local init = function(r, c)
  return 10 * r + c;
end

print ""

m = Matrix.New(5, 5, init);

print(m);

print ""

--print(m:submatrix(1, 1, 5, 5));
print(m:submatrix(2, 2, 3, 3));

print ""

print "==Matrix.FromTable=="
m = Matrix.FromTable{{1, 2},{3, 4}};

print("rows="..m._rows);
print("cols="..m._cols);
print("Storage: ");
for k, v in ipairs(m._storage) do
  print(""..k..": "..v);
end

print(m[2][1])

m[2][1] = 3.14;

print(m[2][1]);

print(tostring(m));

print("==transpose=");
print("original");
m = Matrix.New(3, 3, function(r, c) return 10 * r + c; end);
print(m);
print("transposed:")
print(m:transpose());









