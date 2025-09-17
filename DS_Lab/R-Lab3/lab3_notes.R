# MATRIX() IN R

# matrix(data, nrow, ncol)

matrix(c(1:6), 2, 3)                # Fills column-by-column
matrix(c(1:6), 2, 3, byrow = TRUE)  # Fills row-by-row

# Vector can be transformed into a matrix
matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), nrow = 2, ncol = 3)



# Giving labels to matrix
mat1 <- matrix(c(1:6), 2, 3, byrow = TRUE)
mat1

rownames(mat1)
colnames(mat1)

rownames(mat1) <- c("row1", "row2")
colnames(mat1) <- c("col1", "col2", "col3")
mat1
mat1["row2",]
mat1[,"col3"]
mat1["row1", "col2"]
