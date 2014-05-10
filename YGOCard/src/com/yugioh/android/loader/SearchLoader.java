package com.yugioh.android.loader;

import android.content.Context;
import android.database.Cursor;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseCursorLoader;
import com.yugioh.android.database.YugiohUtils;

public class SearchLoader extends BaseCursorLoader {

    private Bundle bn;

    public SearchLoader(Context context, Bundle bn) {
        super(context);
        this.bn = bn;
    }

    public void setBundle(Bundle bn) {
        this.bn = bn;
    }

    @Override
    public Cursor loadInBackground() {
        Cursor cSearchResult = null;
        if (bn.containsKey("ids")) {
            int[] ids = bn.getIntArray("ids");
            cSearchResult = YugiohUtils.getCardsViaIds(getContext(), ids);
        } else {
            String cardType = bn.getString("cardType");
            String cardAttribute = bn.getString("cardAttribute");
            int cardLevel = bn.getInt("cardLevel");
            String cardRace = bn.getString("cardRace");
            String cardName = bn.getString("cardName");
            String cardEffect = bn.getString("cardEffect");
            String cardAtk = bn.getString("cardAtk");
            String cardDef = bn.getString("cardDef");
            String cardRare = bn.getString("cardRare");
            String cardBelongs = bn.getString("cardBelongs");
            String cardLimit = bn.getString("cardLimit");
            String cardTunner = bn.getString("cardTunner");
            String cardEffectText = bn.getString("cardEffectText");
            cSearchResult = YugiohUtils.getCards(getContext(), cardType, cardAttribute, cardLevel, cardRace, cardName, cardEffect, cardAtk, cardDef, cardRare, cardBelongs, cardLimit, cardTunner, cardEffectText);
        }

        return cSearchResult;
    }


}
