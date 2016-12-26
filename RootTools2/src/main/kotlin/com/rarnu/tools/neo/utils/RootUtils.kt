package com.rarnu.tools.neo.utils

import java.io.BufferedReader
import java.io.DataOutputStream
import java.io.InputStreamReader

object RootUtils {

    data class CommandResult(var result: String, var error: String)

    interface CommandCallback {
        fun onStart()
        fun onReadLine(line: String?)
        fun onReadError(line: String?)
        fun onFinish()
    }

    fun runCommand(command: String, root: Boolean): CommandResult = runCommand(arrayOf(command), root, null)

    fun runCommand(prog: Array<String>): CommandResult = runCommand(prog, false, null)

    fun runCommand(prog: Array<String>, listener: CommandCallback?): CommandResult = runCommand(prog, false, listener)

    fun runCommand(command: String, root: Boolean, listener: CommandCallback?): CommandResult = runCommand(arrayOf(command), root, listener)

    fun runCommand(command: Array<String>, root: Boolean, listener: CommandCallback?): CommandResult {
        val ret = CommandResult("", "")
        var process: Process?
        var rootOs: DataOutputStream? = null
        var procOutOs: BufferedReader? = null
        var procErrOs: BufferedReader? = null
        listener?.onStart()
        try {
            if (root) {
                process = Runtime.getRuntime().exec("su")
                rootOs = DataOutputStream(process.outputStream)
                rootOs.writeBytes("${command[0]}\n")
                rootOs.writeBytes("exit\n")
                rootOs.flush()
            } else {
                if (command.size == 1) {
                    process = Runtime.getRuntime().exec(command[0])
                } else {
                    process = Runtime.getRuntime().exec(command)
                }
            }
            procOutOs = BufferedReader(InputStreamReader(process.inputStream))
            procErrOs = BufferedReader(InputStreamReader(process.errorStream))
            var outStr = StringBuffer()
            var errStr = StringBuffer()
            var line: String?
            while (true) {
                line = procOutOs.readLine()
                if (line != null) {
                    outStr.append("${line}\n")
                    listener?.onReadLine(line)
                } else {
                    break
                }
            }
            while (true) {
                line = procErrOs.readLine()
                if (line != null) {
                    errStr.append("${line}\n")
                    listener?.onReadError(line)
                } else {
                    break
                }
            }
            process.waitFor()
            ret.result = outStr.toString().trim { it <= ' ' }
            ret.error = errStr.toString().trim { it <= ' ' }
        } catch(e: Exception) {
            if (e.message != null) {
                ret.error = if (e.message == null) "" else e.message!!
            }
        } finally {
            rootOs?.close()
            procOutOs?.close()
            procErrOs?.close()
        }
        listener?.onFinish()
        return ret
    }

}
