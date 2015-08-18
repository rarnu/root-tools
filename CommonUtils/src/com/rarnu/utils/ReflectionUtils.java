package com.rarnu.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class ReflectionUtils {

    public static Field[] getClassFields(Object obj) {
        Class<?> clz = obj.getClass();
        Field[] fs = clz.getFields();
        return fs;
    }

    public static Method[] getClassMethods(Object obj) {
        Class<?> clz = obj.getClass();
        Method[] ms = clz.getMethods();
        return ms;
    }

    public static Object invokeClassMethod(Object obj, String method, Object ... params) {
        Object ret = null;
        try {
            Class<?> clz = obj.getClass();
            Method m = clz.getMethod(method);
            ret = m.invoke(obj, params);
        } catch (Exception e) {

        }
        return ret;

    }

    public static Object getClassPrivateFieldValue(Object obj, String field) {
        Object ret =  null;
        try {
            Class<?> clz = obj.getClass();
            Field f = clz.getDeclaredField(field);
            f.setAccessible(true);
            ret = f.get(obj);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return ret;
    }

    public static void setClassPrivateFieldValue(Object obj, String field, Object val) {
        try {
            Class<?> clz = obj.getClass();
            Field f = clz.getDeclaredField(field);
            f.setAccessible(true);
            f.set(obj, val);
        } catch (Exception e) {

        }
    }

    public static Object invokeClassPrivateMethod(Object obj, String method, Object ... params) {
        Object ret = null;
        try {
            Class<?> clz = obj.getClass();
            Method m = clz.getMethod(method);
            m.setAccessible(true);
            ret = m.invoke(obj, params);
        } catch (Exception e) {

        }
        return ret;
    }

}
