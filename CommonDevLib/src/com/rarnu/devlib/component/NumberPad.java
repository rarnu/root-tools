package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;
import com.rarnu.devlib.intf.NumberPadListener;

public class NumberPad extends RelativeLayout implements View.OnClickListener {

    View innerView;
    Button[] btns = null;
    ImageView btnBack;
    NumberPadListener numberListener;

    public NumberPad(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public NumberPad(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public NumberPad(Context context) {
        super(context);
        init();
    }

    public void setNumberPadListener(NumberPadListener listener) {
        this.numberListener = listener;
    }

    private void init() {
        innerView = inflate(getContext(), R.layout.number_pad, null);
        LayoutParams lp = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        innerView.setLayoutParams(lp);
        addView(innerView);

        btns = new Button[11];
        btns[0] = (Button) innerView.findViewById(R.id.btn1);
        btns[1] = (Button) innerView.findViewById(R.id.btn2);
        btns[2] = (Button) innerView.findViewById(R.id.btn3);
        btns[3] = (Button) innerView.findViewById(R.id.btn4);
        btns[4] = (Button) innerView.findViewById(R.id.btn5);
        btns[5] = (Button) innerView.findViewById(R.id.btn6);
        btns[6] = (Button) innerView.findViewById(R.id.btn7);
        btns[7] = (Button) innerView.findViewById(R.id.btn8);
        btns[8] = (Button) innerView.findViewById(R.id.btn9);
        btns[9] = (Button) innerView.findViewById(R.id.btn10);
        btns[10] = (Button) innerView.findViewById(R.id.btn11);
        btnBack = (ImageView) innerView.findViewById(R.id.btnBack);

        for (Button b : btns) {
            b.setOnClickListener(this);
        }
        btnBack.setOnClickListener(this);
    }

    @Override
    public void onClick(View v) {
        if (v instanceof ImageView) {
            if (numberListener != null) {
                numberListener.onBackClick();
            }
        } else if (v instanceof Button) {
            String number = ((Button) v).getText().toString();
            if (numberListener != null) {
                numberListener.onNumberClick(number);
            }
        }

    }
}
