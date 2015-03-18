package com.yugioh.android.classes;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class CardInfo implements Serializable {

    private int _id;
    private String japName;
    private String name;
    private String enName;
    private String sCardType;
    private String CardDType;
    private String tribe;
    private String _package;
    private String element;
    private int level;
    private String infrequence;
    private int atkValue;
    private String atk;
    private int defValue;
    private String def;
    private String effect;
    private String ban;
    private String cheatcode;
    private String adjust;
    private String cardCamp;
    private String oldName;
    private String shortName;
    private int pendulumL;
    private int pendulumR;

    public String toString() {

        String ret = "";
        try {
            Class<?> cls = this.getClass();
            Field[] fs = cls.getDeclaredFields();

            Method mGet = null;
            for (Field f : fs) {
                mGet = cls.getMethod("get" + f.getName(), new Class[]{});
                if (f.getType().getName().contains("String")) {
                    ret = ret + f.getName() + ": "
                            + ((String) mGet.invoke(this, new Object[]{}));
                } else {
                    ret = ret
                            + f.getName()
                            + ": "
                            + ((Integer) mGet.invoke(this, new Object[]{}))
                            .toString();
                }
                ret = ret + "\n";

            }
        } catch (Exception e) {

        }

        return ret;
    }

    public int getId() {
        return _id;
    }

    public void setId(int _id) {
        this._id = _id;
    }

    public String getJapName() {
        return japName;
    }

    public void setJapName(String japName) {
        this.japName = japName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getEnName() {
        return enName;
    }

    public void setEnName(String enName) {
        this.enName = enName;
    }

    public String getSCardType() {
        return sCardType;
    }

    public void setSCardType(String sCardType) {
        this.sCardType = sCardType;
    }

    public String getCardDType() {
        return CardDType;
    }

    public void setCardDType(String cardDType) {
        CardDType = cardDType;
    }

    public String getTribe() {
        return tribe;
    }

    public void setTribe(String tribe) {
        this.tribe = tribe;
    }

    public String getPackage() {
        return _package;
    }

    public void setPackage(String _package) {
        this._package = _package;
    }

    public String getElement() {
        return element;
    }

    public void setElement(String element) {
        this.element = element;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }

    public String getInfrequence() {
        return infrequence;
    }

    public void setInfrequence(String infrequence) {
        this.infrequence = infrequence;
    }

    public int getAtkValue() {
        return atkValue;
    }

    public void setAtkValue(int atkValue) {
        this.atkValue = atkValue;
    }

    public String getAtk() {
        return atk;
    }

    public void setAtk(String atk) {
        this.atk = atk;
    }

    public int getDefValue() {
        return defValue;
    }

    public void setDefValue(int defValue) {
        this.defValue = defValue;
    }

    public String getDef() {
        return def;
    }

    public void setDef(String def) {
        this.def = def;
    }

    public String getEffect() {
        return effect;
    }

    public void setEffect(String effect) {
        this.effect = effect;
    }

    public String getBan() {
        return ban;
    }

    public void setBan(String ban) {
        this.ban = ban;
    }

    public String getCheatcode() {
        return cheatcode;
    }

    public void setCheatcode(String cheatcode) {
        this.cheatcode = cheatcode;
    }

    public String getAdjust() {
        return adjust;
    }

    public void setAdjust(String adjust) {
        this.adjust = adjust;
    }

    public String getCardCamp() {
        return cardCamp;
    }

    public void setCardCamp(String cardCamp) {
        this.cardCamp = cardCamp;
    }

    public String getOldName() {
        return oldName;
    }

    public void setOldName(String oldName) {
        this.oldName = oldName;
    }

    public String getShortName() {
        return shortName;
    }

    public void setShortName(String shortName) {
        this.shortName = shortName;
    }

    public int getPendulumL() {
        return pendulumL;
    }

    public void setPendulumL(int pendulumL) {
        this.pendulumL = pendulumL;
    }

    public int getPendulumR() {
        return pendulumR;
    }

    public void setPendulumR(int pendulumR) {
        this.pendulumR = pendulumR;
    }
}
