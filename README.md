**Timer-8bit**
8-bit counter/timer, Used APB bus for this design and synchronous design
Supports incrementing (count up) or decrementing (count down) of 8-bit values.
Can operate in counting modes with different internal clock frequencies (PCLKx1,PCLKx2, PCLKx4,PCLKx8).
Includes the following registers:
•	TCR (Timer Control Register): Controls the operation of the timer.
•	TDR (Timer Data Register): Stores the initial preload value.
•	TSR (Timer Status Register): Displays the status of the timer.
Detects overflow when counting up from 255 and underflow when counting down from 0.

[Block diagram]
<img width="624" height="258" alt="image" src="https://github.com/user-attachments/assets/89b19999-4d58-4a4c-b4a8-81688a69043d" />
<img width="624" height="267" alt="image" src="https://github.com/user-attachments/assets/8ed5acfc-86aa-404d-984b-35f7d05dcd5c" />
<img width="665" height="310" alt="image" src="https://github.com/user-attachments/assets/de6e94fc-a75a-4dd5-9579-6af561614550" />

