<html>
    <head>
        <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" type="text/css">
        <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"></script>
        <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.bundle.min.js"></script>
   
    </head>
    <style>
        .center{
            text-align: center;
        }
        table{
            width:200px;
            
        }
        th,td{
            padding: 5px;
            border: 2px solid red;
        }
        .bg{
            background-color: #D81C00;
            color: white;
        }
    </style>
<body>
    
    <table class='table table-hover table-responsive'>
        
        <?php
        

	$cricketMatchesTxt = file_get_contents('https://cricapi.com/api/cricketScore?apikey=2JuwflZityd8vCAWVL52VsipKIZ2&unique_id=1034809');	// change with your API key
	$cricketMatches = json_decode($cricketMatchesTxt);

    foreach($cricketMatches->data as $item) {

	 echo "<tr><td>".($item->{'title'})."</td></tr>";  }
    ?>  </table>
    
</body>
</html>
