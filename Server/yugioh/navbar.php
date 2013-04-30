
<head>
<meta http-equiv="Content-Language" content="en-us">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<title>YuGiOh Information Page</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content>
<meta name="author" content>
<!-- Le styles -->
<style>
      body {
        padding-bottom: 30px;
      }
      .hero-unit {
        margin-top: 20px;
      }
</style>

<link href="../common/bootstrap/css/bootstrap.css" rel="stylesheet">
<link href="../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>

  <body>

    <div class="container">

      <!-- Static navbar -->
      <div class="navbar">
        <div class="navbar-inner">
          <div class="container">
            <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
            </a>
            <a class="brand" href="page_info.php">YuGiOh Information</a>
            <div class="nav-collapse collapse">
              <ul class="nav">
		
                <li><a href="page_info.php">Information</a></li>
		<?php 
			$admin = $_GET["is_admin"];
			if ($admin==="king") {
				echo "<li><a href=\"page_add.php\">Add</a></li>";
				echo "<li><a href=\"page_delete.php\">Delete</a></li>";
			}
		 ?>
              </ul>

            </div><!--/.nav-collapse -->
          </div>
        </div>
      </div>



    </div> <!-- /container -->

    <!-- Le javascript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    <script src="../common/bootstrap/js/jquery.js"></script>
    <script src="../common/bootstrap/js/bootstrap-transition.js"></script>
    <script src="../common/bootstrap/js/bootstrap-alert.js"></script>
    <script src="../common/bootstrap/js/bootstrap-modal.js"></script>
    <script src="../common/bootstrap/js/bootstrap-dropdown.js"></script>
    <script src="../common/bootstrap/js/bootstrap-scrollspy.js"></script>
    <script src="../common/bootstrap/js/bootstrap-tab.js"></script>
    <script src="../common/bootstrap/js/bootstrap-tooltip.js"></script>
    <script src="../common/bootstrap/js/bootstrap-popover.js"></script>
    <script src="../common/bootstrap/js/bootstrap-button.js"></script>
    <script src="../common/bootstrap/js/bootstrap-collapse.js"></script>
    <script src="../common/bootstrap/js/bootstrap-carousel.js"></script>
    <script src="../common/bootstrap/js/bootstrap-typeahead.js"></script>

  </body>
