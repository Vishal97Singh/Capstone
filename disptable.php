<!DOCTYPE html>
<html>
<head>
 <title>Table with database</title>
 <style>
  table {
   border-collapse: collapse;
   width: 100%;
   color: #588c7e;
   font-family: monospace;
   font-size: 25px;
   text-align: left;
     } 
  th {
   background-color: #588c7e;
   color: white;
    }
  tr:nth-child(even) {background-color: #f2f2f2}
 </style>
</head>
<body>
 <table>
 <tr>
  <th>Year</th> 
  <th>Mat</th> 
  <th>Inns</th>
  <th>NO</th>
<th>100s</th>
<th>50s</th>
<th>0s</th>
<th>HS</th>
<th>Runs</th>
     <th>Avg</th>
<th>S/R</th>
<th>Ca</th>
<th>St</th>

 </tr>
 <?php
$conn = mysqli_connect("localhost", "root", "", "cricketrecords");
  // Check connection
  if ($conn->connect_error) {
   die("Connection failed: " . $conn->connect_error);
  } 
  $sql = "SELECT * FROM `adam gilchrist`";
  $result = $conn->query($sql);
  if ($result->num_rows > 0) {
   // output data of each row
   while($row = $result->fetch_assoc()) {
    echo "<tr><td>" . $row["Year"]. "</td><td>" . $row["Mat"] . "</td><td>". $row["Inns"]. "</td><td>" . $row["NO"] . "</td><td>" . $row["100s"] . "</td><td>" . $row["50s"] . "</td><td>" . $row["0s"] . "</td><td>" . $row["HS"] . "</td><td>" . $row["Runs"] . "</td><td>" . $row["Avg"] . "</td><td>" . $row["S/R"] . "</td><td>" . $row["Ca"] . "</td><td>" . $row["St"] . "</td></tr>";
}
echo "</table>";
} else { echo "0 results"; }
$conn->close();
?>
</table>
</body>
</html>