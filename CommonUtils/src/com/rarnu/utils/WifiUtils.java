package com.rarnu.utils;

import android.content.Context;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;

import java.lang.reflect.Method;
import java.util.List;

public class WifiUtils {

    WifiManager.WifiLock mWifiLock;
    private WifiManager mWifiManager;
    private WifiInfo mWifiInfo;
    private List<ScanResult> mWifiList;
    private List<WifiConfiguration> mWifiConfigurations;

    public WifiUtils(Context context) {
        mWifiManager = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
        mWifiInfo = mWifiManager.getConnectionInfo();
    }

    public void openWifi() {
        if (!mWifiManager.isWifiEnabled()) {
            mWifiManager.setWifiEnabled(true);
        }
    }

    public void closeWifi() {
        if (!mWifiManager.isWifiEnabled()) {
            mWifiManager.setWifiEnabled(false);
        }
    }

    public int checkState() {
        return mWifiManager.getWifiState();
    }

    public void acquireWifiLock() {
        mWifiLock.acquire();
    }

    public void releaseWifiLock() {
        if (mWifiLock.isHeld()) {
            mWifiLock.acquire();
        }
    }

    public void createWifiLock(String name) {
        mWifiLock = mWifiManager.createWifiLock(name);
    }

    public List<WifiConfiguration> getConfiguration() {
        return mWifiConfigurations;
    }

    public void connetionConfiguration(int index) {
        if (index > mWifiConfigurations.size()) {
            return;
        }
        mWifiManager.enableNetwork(mWifiConfigurations.get(index).networkId, true);
    }

    public void startScan() {
        mWifiManager.startScan();
        mWifiList = mWifiManager.getScanResults();
        mWifiConfigurations = mWifiManager.getConfiguredNetworks();
    }

    public List<ScanResult> getWifiList() {
        mWifiList = mWifiManager.getScanResults();
        mWifiConfigurations = mWifiManager.getConfiguredNetworks();
        return mWifiList;
    }

    public String getMacAddress() {
        return (mWifiInfo == null) ? "NULL" : mWifiInfo.getMacAddress();
    }

    public String getBSSID() {
        return (mWifiInfo == null) ? "NULL" : mWifiInfo.getBSSID();
    }

    public String getSSID() {
        return (mWifiInfo == null) ? "NULL" : mWifiInfo.getSSID();
    }

    public int getIpAddress() {
        return (mWifiInfo == null) ? 0 : mWifiInfo.getIpAddress();
    }

    public int getNetWordId() {
        return (mWifiInfo == null) ? 0 : mWifiInfo.getNetworkId();
    }

    public WifiInfo getWifiInfo() {
        mWifiInfo = mWifiManager.getConnectionInfo();
        return mWifiInfo;
    }

    public int addNetWork(WifiConfiguration configuration) {
        int wcgId = mWifiManager.addNetwork(configuration);
        boolean isSuccess = mWifiManager.enableNetwork(wcgId, true);
        return isSuccess ? wcgId : -1;
    }

    public void connectWifi(int netId) {
        mWifiManager.enableNetwork(netId, true);
    }

    public void disConnectionWifi(int netId) {
        mWifiManager.disableNetwork(netId);
        mWifiManager.disconnect();
    }

    public WifiConfiguration createWifiInfo(String ssid, String password, int Type) {
        WifiConfiguration config = new WifiConfiguration();
        config.allowedAuthAlgorithms.clear();
        config.allowedGroupCiphers.clear();
        config.allowedKeyManagement.clear();
        config.allowedPairwiseCiphers.clear();
        config.allowedProtocols.clear();
        config.SSID = "\"" + ssid + "\"";

        WifiConfiguration tempConfig = isExsits(ssid);
        if (tempConfig != null) {
            mWifiManager.removeNetwork(tempConfig.networkId);
        }

        if (Type == 1) {
            config.wepKeys[0] = "";
            config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
            config.wepTxKeyIndex = 0;
        }
        if (Type == 2) {
            config.hiddenSSID = true;
            config.wepKeys[0] = "\"" + password + "\"";
            config.allowedAuthAlgorithms.set(WifiConfiguration.AuthAlgorithm.SHARED);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP40);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP104);
            config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
            config.wepTxKeyIndex = 0;
        }
        if (Type == 3) {
            config.preSharedKey = "\"" + password + "\"";
            config.hiddenSSID = true;
            config.allowedAuthAlgorithms.set(WifiConfiguration.AuthAlgorithm.OPEN);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP);
            config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.WPA_PSK);
            config.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.TKIP);
            config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP);
            config.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.CCMP);
            config.status = WifiConfiguration.Status.ENABLED;
        }
        return config;
    }

    private WifiConfiguration isExsits(String ssid) {
        List<WifiConfiguration> existingConfigs = mWifiManager.getConfiguredNetworks();
        for (WifiConfiguration existingConfig : existingConfigs) {
            if (existingConfig.SSID.equals("\"" + ssid + "\"")) {
                return existingConfig;
            }
        }
        return null;
    }

    public boolean createWifiAp(String ssid, String password, boolean enabled) {
        if (enabled) {
            mWifiManager.setWifiEnabled(false);
        }
        try {
            WifiConfiguration apConfig = new WifiConfiguration();
            apConfig.SSID = ssid;
            apConfig.preSharedKey = password;
            apConfig.hiddenSSID = true;
            apConfig.status = WifiConfiguration.Status.ENABLED;
            apConfig.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP);
            apConfig.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP);
            apConfig.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.WPA_PSK);
            apConfig.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.TKIP);
            apConfig.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.CCMP);
            apConfig.allowedProtocols.set(WifiConfiguration.Protocol.RSN);
            Method m = mWifiManager.getClass().getDeclaredMethod("setWifiApEnabled", WifiConfiguration.class, Boolean.class);
            m.setAccessible(true);
            Boolean ret = (Boolean) m.invoke(mWifiManager, apConfig, new Boolean(enabled));
            return ret.booleanValue();
        } catch (Exception e) {
            return false;
        }
    }

    public void removeWifi(int netId) {
        mWifiManager.removeNetwork(netId);
    }

    public static boolean getMeteredHint(WifiInfo info) {
        boolean ret = false;
        try {
            Method m = info.getClass().getDeclaredMethod("getMeteredHint");
            m.setAccessible(true);
            ret = (Boolean) m.invoke(info);
        } catch (Exception e) {

        }
        return ret;
    }
}
