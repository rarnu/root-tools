package com.rarnu.tools.neo.utils

import okhttp3.*
import java.io.*
import java.net.URL
import java.util.concurrent.TimeUnit

/**
 * Created by rarnu on 3/25/16.
 */
object HttpUtils {

    data class HttpRequestResponseData(var cookie: CookieJar?, var data: String?): Serializable

    fun simplePostWithHeader(host: String, param: String, encoding: String, property: Map<String, String?>?): String {
        val url = URL(host)
        val conn = url.openConnection()
        if (property != null) {
            val iter = property.entries.iterator()
            while (iter.hasNext()) {
                val entry = iter.next()
                conn.addRequestProperty(entry.key, entry.value)
            }
        }
        conn.doOutput = true
        val osw = OutputStreamWriter(conn.outputStream)
        osw.write(param)
        osw.flush()
        osw.close()
        val isr = InputStreamReader(conn.inputStream, encoding)
        val br = BufferedReader(isr)
        var content: String?
        var result = ""
        while(true) {
            content = br.readLine()
            if (content != null) {
                result += "$content\n"
            } else {
                break
            }
        }
        br.close()
        isr.close()
        return result
    }

    fun simplePost(host: String, param: String, encoding: String): String = simplePostWithHeader(host, param, encoding, null)

    fun post(host: String?, getParams: String?, params: Map<String, String?>?): String? {
        val url = "$host?$getParams"
        return post(url, params)
    }

    fun post(host: String?, params: Map<String, String?>?): String? {
        val body = buildBody(params)
        val req = Request.Builder().url(host).post(body).build()
        return executeForResult(req)
    }

    fun postFile(host: String?, params: Map<String, String?>?, files: Map<String, String?>?): String? {
        val body = buildPostFileParts(params, files)
        val req = Request.Builder().url(host).post(body).build()
        return executeForResult(req)
    }

    fun postWithCookie(host: String?, params: Map<String, String?>?, cookie: CookieJar?): HttpRequestResponseData? {
        val body = buildBody(params)
        val req = Request.Builder().url(host).post(body).build()
        return executeForData(req, cookie)
    }


    fun postFileWithCookie(host: String?, params: Map<String, String?>?, files: Map<String, String?>?, cookie: CookieJar?): HttpRequestResponseData? {
        val body = buildPostFileParts(params, files)
        val req = Request.Builder().url(host).post(body).build()
        return executeForData(req, cookie)
    }

    private fun buildPostFileParts(params: Map<String, String?>?, files: Map<String, String?>?): RequestBody? {
        val builder = MultipartBody.Builder()
        builder.setType(MultipartBody.FORM)
        val iterParam = params?.keys?.iterator()
        while (iterParam!!.hasNext()) {
            val key = iterParam.next()
            val value = params!![key]
            builder.addFormDataPart(key, value)
        }
        val iterFile = files?.keys?.iterator()
        while (iterFile!!.hasNext()) {
            val key = iterFile.next()
            val file = files!![key]
            val f = File(file)
            val fileBody = RequestBody.create(MediaType.parse("application/octet-stream"), f)
            builder.addFormDataPart(key, file?.substring(file.lastIndexOf("/") + 1), fileBody)
        }
        return builder.build()
    }

    fun get(host: String?, params: String?): String? {
        val req = Request.Builder().url("$host?$params").build()
        return executeForResult(req)
    }

    fun delete(host: String?, params: Map<String, String?>?): String? {
        val body = buildBody(params)
        val req = Request.Builder().url(host).delete(body).build()
        return executeForResult(req)
    }

    fun getWithCookie(host: String?, params: String?, cookie: CookieJar?): HttpRequestResponseData? {
        val req = Request.Builder().url("$host?$params").build()
        return executeForData(req, cookie)
    }


    private fun buildBody(params: Map<String, String?>?): RequestBody? {
        val builder = FormBody.Builder()
        val iter = params?.keys?.iterator()
        while (iter!!.hasNext()) {
            val key = iter.next()
            val value = params!![key]
            builder.add(key, value)
        }
        return builder.build()
    }

    private fun executeForResult(req: Request?): String? {
        val http = OkHttpClient.Builder().connectTimeout(10, TimeUnit.SECONDS).build()
        val call = http.newCall(req)
        var ret = ""
        try {
            val resp = call.execute()
            if (resp.isSuccessful) {
                ret = resp.body().string()
            }
        } catch (e: Exception) {

        }
        return ret
    }

    private fun executeForData(req: Request?, cookie: CookieJar?): HttpRequestResponseData? {
        var data: HttpRequestResponseData? = null
        val builder = OkHttpClient.Builder()
        builder.connectTimeout(10, TimeUnit.SECONDS)
        if (cookie != null) {
            builder.cookieJar(cookie)
        }
        val http = builder.build()
        val call = http.newCall(req)
        try {
            val resp = call.execute()
            if (resp.isSuccessful) {
                data = HttpRequestResponseData(http.cookieJar(), resp.body().string())
            }
        } catch (e: Exception) {

        }
        return data

    }

}