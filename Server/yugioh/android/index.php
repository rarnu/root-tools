<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Language" content="en-us">
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title></title>
    <link href="../../common/bootstrap/css/bootstrap.css" rel="stylesheet">
    <link href="../../common/bootstrap/css/bootstrap-responsive.css" rel="stylesheet">

</head>
<body>

<div class="container">
    <div class="hero-unit">

        <form id="form_ygo" class="form-horizontal" method="post" action="import_data.php" enctype="multipart/form-data">

            <?php $ver = shell_exec("python ./yugioh_import.py R"); ?>
            <input type="hidden" name="version" value="<?php echo $ver ?>">
            <table border="0" width="80%" cellpadding="8">

                <tr>
                    <td width="25%">当前版本号</td>
                    <td><?php echo $ver ?></td>
                </tr>
                <tr>
                    <td width="25%">上传新版本MDB数据库</td>
                    <td><input type="file" name="mdb" class="input-block-level"></td>
                </tr>
                <tr>
                    <td width="25%"></td>
                    <td><input type="submit" value="提交" class="btn btn-primary">&nbsp;&nbsp;<input type="reset" value="重置" class="btn"></td>
                </tr>
            </table>

        </form>

        <p>
            下载当前数据库：<a href="yugioh.db">点击下载</a>
        </p>

    </div>
</div>

<script src="../../common/bootstrap/js/jquery.js"></script>
<script src="../../common/bootstrap/js/bootstrap-transition.js"></script>
<script src="../../common/bootstrap/js/bootstrap-alert.js"></script>
<script src="../../common/bootstrap/js/bootstrap-modal.js"></script>
<script src="../../common/bootstrap/js/bootstrap-dropdown.js"></script>
<script src="../../common/bootstrap/js/bootstrap-scrollspy.js"></script>
<script src="../../common/bootstrap/js/bootstrap-tab.js"></script>
<script src="../../common/bootstrap/js/bootstrap-tooltip.js"></script>
<script src="../../common/bootstrap/js/bootstrap-popover.js"></script>
<script src="../../common/bootstrap/js/bootstrap-button.js"></script>
<script src="../../common/bootstrap/js/bootstrap-collapse.js"></script>
<script src="../../common/bootstrap/js/bootstrap-carousel.js"></script>
<script src="../../common/bootstrap/js/bootstrap-typeahead.js"></script>
</body>
</html>