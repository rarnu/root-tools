package com.rarnu.tools.neo.utils

import android.graphics.Bitmap

import java.io.File
import java.io.FileOutputStream

/**
 * Created by rarnu on 11/21/16.
 */
object ImageUtils {

    fun saveBitmapToFile(bmp: Bitmap?, fileName: String?) {
        if (fileName != null && fileName != "") {
            try {
                val f = File(fileName)
                f.createNewFile()
                val fOut = FileOutputStream(f)
                bmp?.compress(Bitmap.CompressFormat.PNG, 100, fOut)
                fOut.flush()
                fOut.close()
            } catch (e: Exception) {

            }

        }
    }

}
