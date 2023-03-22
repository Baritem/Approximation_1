#include <gtkmm.h>
#include <vector>
#include <memory>
#include <string>
#include <iostream>
#include <ctime>
#include <numbers>
#include <windows.h>
#include <thread>
#include <bitset>
#include <condition_variable>
#include <map>
#include <chrono>
using namespace std;
long long tree_depth = 4;
int crossover_type = 0;
constexpr long long MIN_A = 0;
constexpr long long MAX_B = 100;
constexpr long long var_length = 16;
constexpr long long bin_max = 2 << (var_length - 1);
long long individuals = 500;
long long generations = 5000;
constexpr long long max_mutations = 8;
constexpr long long max_opts = 6;
long long threads = 12;
constexpr long long golden_accuracy = 1;
long long variables = 2;
constexpr double phi = 5;
bool show_debug = false;
bool use_gradient = true;
bool ignore_generations = false;
constexpr chrono::milliseconds to_sleep(25);
//bool SHOW = false;
bool DO_t1 = true;
template <typename T>
class SafeVector {
private:
    vector<T> vec;
    mutex mut;
public:
    void push_back(T input) {
        lock_guard<mutex> loc(mut);
        vec.push_back(input);
    }
    vector<T> to_vector() {
        return vec;
    }
    void clear() {
        vec = {};
    }
};
struct Point {
    float x = 0;
    float y = 0;
    float z = 0;
    map<string, double> x1;
    //Point(float x, float z): x(x), z(z){}
   // Point(float x, float y, float z): x(x), y(y), z(z){}
    Point(double z1, vector<double> x2){
        this->z = z1;
        for (int i = 0; i < x2.size(); i++) {
            x1["x" + to_string(i + 1)] = x2[i];
        }
    }
};
float gray(long long n) {
    return n ^ (n >> 1);
}
float graytobin(long long n) {
    long long bin = 0;
    for (bin = 0; n; n >>= 1) {
        bin ^= n;
    }
    return bin;
}
bool isnumber(string s) {
    for (int i = 0; i < s.size(); i++) {
        if (!(s[i] == '0'|| s[i]=='1' || s[i] == '.')) {
            return false;
        }
    }
    return true;
}
vector<string> operations = { "+", "-", "*", "/", "sin", "cos", "sqrt", "abs", "return_0", "return_1", "cut_l", "cut_r"};
vector<double> consts = { 0, 1, 2,3, 5, 10, 100, numbers::e, numbers::pi };
struct Individual {
    double fitness = 0;
    struct Tree{
    Tree * right = nullptr;
    Tree* left = nullptr;
    string val = "";
    bool isConst = false;
    Tree() {}
    Tree(Tree* tree) { val = tree->val; isConst = tree->isConst; if (tree->left) { right = new Tree(tree->right); left = new Tree(tree->left); } }
    ~Tree() { delete(right); delete(left); }
    void generate_tree(int n, Tree* node = nullptr) {
        if (!node)
            node = this;
        if (n == 0) {
            if (rand() % 10 == 1) {
                this->val = "x" + to_string(rand() % variables + 1);
                return;
                #ifdef USE_Y
                if (rand % 2 == 1)
                    this->val = "y";
                #endif 

            }
            else if (rand() % 10 == 1) {
                this->val = consts[rand() % consts.size()];
                this->isConst = true;
            }
            else {
                string temp = "";
                for (int i = 0; i < var_length; i++) {
                    temp += rand() % 2 + '0';
                }
                this->val = temp;
            }
        }
        else {
            this->val = operations[rand() % operations.size()];
        }
        if (n > 0) {
            this->left = new Tree();
            this->right = new Tree();
            this->left->generate_tree(n - 1);
            this->right->generate_tree(n - 1);
        }
    }
    Tree* select_rnd_node(int& count, Tree* selected = nullptr) {
        if (rand() % count + 1 == count)
            selected = this;
        count += 1;
        if (this->left) {
            selected = this->left->select_rnd_node(count, selected);
            selected = this->right->select_rnd_node(count, selected);
        }
        return selected;
    }
    double calculate_tree(float x, map<string, double>& x1, float y = 0) {
        if (this->left == nullptr) {
            //if (this->val == "x")
            //    return x;
            //if (this->val == "y")
            //    return y;
            if (this->isConst) {
                return stof(this->val);
            }
            if (!isnumber(this->val))
                return x1[val];
            double temp = float(2 << (var_length - 1));
            if (temp == 0) {
                return 1000000000;
            }

            
            return ((MAX_B - MIN_A) / temp) * graytobin(stoi(this->val, 0, 2)) + MIN_A;
        }
        double right = this->right->calculate_tree(x, x1);
        double left = this->left->calculate_tree(x, x1);
        if (this->val == "+") {
            return left + right;
        }
        else if (this->val == "-") {
            return left - right;
        }
        else if (this->val == "*") {
            return left * right;
        }
        else if (this->val == "sqrt") {
            if (right + left < 0)
                throw invalid_argument("");
            return sqrt(left + right);
        }
        else if (this->val == "sin") {
            return sin(left + right);
        }
        else if (this->val == "cos") {
            return cos(left + right);
        }
        else if (this->val == "abs") {
            return abs(left + right);
        }
        else if (this->val == "return_0") {
            return 0;
        }
        else if (this->val == "return_1") {
            return 1;
        }
        else if (this->val == "cut_l") {
            return right;
        }
        else if (this->val == "cut_r") {
            return left;
        }
        else if (this->val == "/") {
            if (right == 0) {
                throw invalid_argument("x/0");
                return 1000000000;
            }
            return left / right;
        }
    }
    void nodeToX() {
        this->val = "x" + to_string(rand() % variables + 1);
        this->isConst = false;
    }
    void nodeToBin() {
        string temp(var_length, ' ');
        for (int i = 0; i < var_length; i++) {
            temp[i] =  rand() % 2 + '0';
        }
        this->val = temp;
        this->isConst = false;
    }
    void nodeToConst() {
        this->val = to_string(consts[rand() % consts.size()]);
        this->isConst = true;
    }
    string print_tree() {
        if (this->left == nullptr) {
            //if (this->val == "x" || this->val == "y")
            if (this->isConst) {
                return (this->val);
            }
            if(!isnumber(this->val))
                return this->val;
            
            return to_string(((MAX_B - MIN_A) / float(2 << (var_length - 1))) * graytobin(stoi(this->val, 0, 2)) + MIN_A);
        }
        if (this ->val == "abs") {
            return "(|" + this->left->print_tree() + "+" + this->right->print_tree() + "|)";
        }
            if (this->val == "cos" || this->val == "sin" || this->val == "sqrt" || this->val == "abs")
                return "(" + this->val + "(" + this->left->print_tree() + "+" + this->right->print_tree() + "))";
            if (this->val == "cut_l")
                return "("+ this->right->print_tree() + ")";
            if (this->val == "cut_r")
                return "("+ this->left->print_tree() + ")";
            if (this->val == "return_0")
                return "(0)";
            if (this->val == "return_1")
                return "(1)";

        return ("(" + this->left->print_tree() + this->val + this->right->print_tree() + ")");
    }
    void golden(vector<Point>& points, Individual* individual = nullptr) {
        if (this->right) {
            if (rand() % 2)
                this->right->golden(points, individual);
            else
                this->left->golden(points, individual);
        }
        else if(isnumber(this->val)) {
            long long a = 0;
            long long b = 2 << (var_length - 1);
            long long x = 0, x1 = 0;
            while (abs(b - a) > golden_accuracy) {
                x = b - (b - a) / phi;
                x1 = a + (b - a) / phi;
                this->val = bitset<var_length>(x).to_string();
                double y = individual->get_fitness(points);
                this->val = bitset<var_length>(x1).to_string();
                double y1 = individual->get_fitness(points);
                if (y <= y1) {
                    a = x;
                }
                else
                    b = x;
            }
        }
    }
    };

    Tree* dna = nullptr;
    double  calc_fitness(vector<Point>& points) {
        double temp = 0;
        try {
            for (int i = 0; i < points.size(); i++) {
                double temp1 = this->dna->calculate_tree(points[i].z, points[i].x1);
                temp += (points[i].z - temp1) * (points[i].z - temp1);
            }
        }
        catch (...) {
            this->fitness = 1000000000;
            return  1000000000;
        }
        this->fitness = temp;
        return temp;
    }
    double get_fitness(vector<Point>& points) {
        return this->fitness;
    }
    void crossover1(Tree* tree, Tree* first, Tree* second) {
        if (rand() % 2) {
            tree->val = first->val;
            tree->isConst = first->isConst;
        }
        else {
            tree->val = second->val;
            tree->isConst = second->isConst;

        }
        if (first->left) {
            tree->left = new Tree();
            tree->right = new Tree();
            crossover1(tree->left, first->left, second->left);
            crossover1(tree->right, first->right, second->right);
        }
    }
    void crossover2(Individual* second_one) {
        int n = rand() % (2 << (tree_depth - 1));
        Tree* first = this->dna;
        Tree* second = second_one->dna;
        while (n > 1) {
            if (n % 2 == 0) {
                first = first->left;
                second = second->left;
            }
            else {
                first = first->right;
                second = second->right;

            }
            n /= 2; 
        }
        if (n % 2 == 0) {
            delete(first->left);
            first->left = new Tree(second->left);
        }
        else{
            delete(first->right);
            first->right = new Tree(second->right);
        }
    }
    void crossover3(Individual* second_one) {
        Tree* first = this->dna;
        Tree* second = second_one->dna;
        int c = 1;
        first = first->select_rnd_node(c);
        c = 1;
        second = second->select_rnd_node(c);
        if (first->left == nullptr)
            return;
        if (rand() % 2 == 1) {
            delete(first->right);
            first->right = new Tree(second);
        } 
        else{
            delete(first->left);
            first->left = new Tree(second);
        }
    }
    void crossover(Individual* second) {
        Tree* tree = nullptr;
        switch (crossover_type) {
        case 0:

                tree = new Tree();
                this->crossover1(tree, this->dna, second->dna);
                delete(this->dna);
                this->dna = tree;
                break;
        case 1:
                this->crossover2(second);
                break;

        case 2:
            this->crossover3(second);
            break;
        }
        return;
    }
    void mutation(vector<int> way = {}, int j = 0, Tree* tree = nullptr) {
        if (way.size() == 0 && (rand() % (2 << (tree_depth)))) {
            int temp = rand() %( 2 << (tree_depth));
            while (temp > 1) {
                way.push_back(temp % 2);
                temp /= 2;
            }
            for (int i = 0; i < way.size()/2; i++) {
                swap(way[i], way[way.size() - 1 - i]);
            }
        }
        if (!tree)
            tree = this->dna;
        if (j == way.size()) {
            if (isnumber(tree->val) || tree->isConst) {
                if (tree->isConst) {
                    if (rand() % 5 == 1) {
                        tree->nodeToX();
                        return;
                    }
                    else if (rand() % 5 == 1) {
                        tree->nodeToConst();
                        return;
                    }
                    else if (rand() % 5 == 1) {
                        tree->nodeToBin();
                    }
#ifdef USE_Y
                    if (rand() % 5 == 1) {
                        tree->val = "y";
                        return;
                    }
#endif 
                }
                else {
                    if (rand() % 5 == 1) {
                        tree->nodeToConst();
                    }
                    else if (rand() % 5 == 1) {
                        tree->nodeToX();
                    }
                    else if (rand() % 5 == 1) {
                        for (int i = 0; i < var_length; i++) {
                            if (rand() % 32 == 1) {
                                tree->val[i] = !(tree->val[i] - '0') + '0';
                            }

                        }
                    }
                            
                }

                
            }
            else if (find(operations.begin(), operations.end(), tree->val) == operations.end()) {
                if (rand() % 5 == 1)
                {
                    tree->val = "x" + to_string(rand() % variables + 1);
                }
                else if (rand() % 5 == 1) {
                    tree->nodeToBin();
                }
                else if (rand() % 5 == 1) {
                    tree->nodeToConst();
                }
            }
                /*else if (tree->val == "x") {
                    if (rand() % 5 == 1) {
                        string temp(var_length, ' ');
                        for (int i = 0; i < var_length; i++) {
                            temp[i] = rand() % 2 + '0';
                        }
                        tree->val = temp;
                        return;
                    }
                    #ifdef USE_Y
                    else if (random() % 5 == 1) {
                        tree->val = "y";
                    }
                    #endif
                }
               //#ifdef USE_Y
                else if (tree->val == "y") {
                    if (rand() % 5 == 1) {
                        tree->val = "x";
                    }
                    else if (rand() % 5 == 1) {
                        string temp(var_length, ' ');
                        for (int i = 0; i < var_length; i++) {
                            temp[i] = rand() % 2 + '0';
                        }
                        tree->val = temp;
                    }

                }
               // #endif */
            
                else {
                    tree->val = operations[rand() % operations.size()];
                }
            }
        
        
        if (j  < way.size() && way[j] == 0) 
            this->mutation(way, j + 1, tree->left);
        if(j < way.size() && way[j] == 1)
            this->mutation(way, j + 1, tree->right);

        

    }
    void golden(vector<Point>& points) {
        this->dna->golden(points, this);
    }
    void opt1(vector<Point>& points, Tree* tree = nullptr) {
        if (!tree)
            tree = this->dna;
        if (tree->left) {
            if (rand() % 2) 
                this->opt1(points, tree->left);
            else
                this->opt1(points, tree->right);
            
        }
        else {
            if (tree->isConst)
                return;
            if (isnumber(tree->val)) {
                double best2 = this->calc_fitness(points);
                long long t1 = graytobin(stoi(tree->val, 0, 2));
                long long t2 = min(t1 + 1, bin_max);
                tree->val = bitset<var_length>(gray(t2)).to_string();
                double f1 = this->calc_fitness(points); 
                t2 = max(t1 - 1, 0);
                tree->val = bitset<var_length>(gray(t2)).to_string();
                double f2 = this->calc_fitness(points);
                int d = -1;
                int s = 1;
                if (f1 < f2) {
                    d = 1;
                }
                long long best1 = t1;
                for (long long j = t1; j >= 0 && j <= bin_max; j += s * d) {
                    tree->val = bitset<var_length>(gray(j)).to_string();
                    double t3 = this->calc_fitness(points);
                    if (best2 >= t3) {
                        best1 = j;
                        best2 = t3;
                    }
                    else {
                        break;
                    }
                }
                tree->val = bitset<var_length>(gray(best1)).to_string();
            }
        }
    }
    Individual* find_best(Individual* second, vector<Point>& points) {
        if (this->get_fitness(points) < second->get_fitness(points)) {
            Individual* temp = new Individual(this);
            return temp;
        }
        Individual* temp = new Individual(second);
        return temp;
    }
    Individual* find_best_no_copy(Individual* second, vector<Point>& points) {
        if (this->get_fitness(points) < second->get_fitness(points)) {
            return this;
        }
        return second;
    }
    Individual(Individual* individual) { this->fitness = individual->fitness; dna = new Tree(individual->dna); }
    Individual() { dna = new Tree();  dna->generate_tree(tree_depth); }
    ~Individual() { delete(dna); }
};
double func(double x, double x1 = 0, double x2 = 0, double x3 = 0, double x4 = 0, double x5 = 0, double x6 = 0, double x7 = 0) {
    //return x * x * x * x;re
    return x * x1;
    //return sqrt(x);
    //return sin(x) + cos(x) - 10 + 5 * x;
}
Individual* best;
vector<Point> points;
unsigned long long t = time(0);
void wait() {
    while (DO_t1) {
        if (GetKeyState(VK_END) & 0x8000) {
           // SHOW = true;
            cout << "DNA: "  << endl << best->dna->print_tree() << endl << "FITNESS: " << best->get_fitness(points) << endl << "TIME: " << time(0) - t << endl;
            //cout << "Wait..." << endl;
            while(GetKeyState(VK_END) & 0x8000){}
        }
        this_thread::sleep_for(to_sleep);
    }

}
long long individuals_todo = individuals;
vector<Individual*> a;
SafeVector<Individual*> b;
void generation() {
    while(individuals_todo > 0){
        individuals_todo -= 1;
        //cout << rand() % var_length << endl;
            Individual* temp = a[rand() % individuals]->find_best(a[rand() % individuals],  points);
            Individual* temp1 = a[rand() % individuals]->find_best_no_copy(a[rand() % individuals],  points);
            temp->crossover(temp1);
            for(int k = rand() % max_mutations + 1; k >=0; k--)
            temp->mutation();
            temp->calc_fitness(points);
            b.push_back(temp);
        }
}
int start()
{
    thread *t1 = new thread();
    if(show_debug)
        t1 = new thread(wait);
    srand(time(0));
    
   //for (int i = 0; i < 10; i++) {
   //     points.push_back(Point(func(i, (i + 13)), {double(i),  (double(i) + 13)}));
    //}
    for (int i = 0; i < individuals; i++) {
        a.push_back( new Individual());
        a[i]->calc_fitness(points);
    }
    
    
    best = a[0];
    
    int i = 0;
    while(best->get_fitness(points) > 0.005 && (ignore_generations || i < generations)){
        individuals_todo = individuals;
   // for (int i = 0; i < geneerations; i++) {
        if (i % 100 == 0) {
            
            cout << i << endl << best->get_fitness(points) << endl;
         //   for(auto x : a)
                for(int k = rand() % max_opts + 1; use_gradient && k >= 0; k--)
                    best->opt1(points);
            //best->golden(points);
            //if (SHOW) {
            //    cout << best->dna->print_tree() << endl;
            //    cout << time(0) - t<< endl;
            //    SHOW = false;
            //}  
        }
        /* for (int j = 0; j < individuals; j++) {
                    //individuals_todo -= 1;
                    //cout << rand() % var_length << endl;
                    Individual* temp = a[rand() % individuals]->find_best(a[rand() % individuals], points);
                    Individual* temp1 = a[rand() % individuals]->find_best_no_copy(a[rand() % individuals], points);
                    temp->crossover(temp1);
                    for (int k = rand() % max_mutations + 1; k >= 0; k--)
                        temp->mutation();
                    temp->calc_fitness(points);
                    b.push_back(temp);
                }*/
        vector<thread*> working_threds(threads);
        for (int thr = 0;  thr< threads; thr++) {
            working_threds[thr] = new thread(generation);
        }
        for (int thr = 0; thr < threads; thr++) {
            working_threds[thr]->join();
            delete(working_threds[thr]);
        }
        for (int j = 0; j < individuals; j++) {
            best = best->find_best_no_copy(a[j], points);
        }
        best = new Individual(best);
        for (int i = 0; i < individuals; i++) {
                delete(a[i]);
           
        }
        a = b.to_vector();
        delete(a[0]);
        a[0] = best;
        b.clear();
        i++;
    }
    cout << best->dna->print_tree() << endl;
    cout << best->get_fitness(points) << endl;
    cout << time(0) - t;
    DO_t1 = false;
    if (show_debug) {
        t1->join();
        delete(t1);
    }
    return 0;
}
//int main() {
 //   start();
//}

class MainWindow : public Gtk::Window {
public:
    MainWindow();
    virtual ~MainWindow(){};

protected:
    void on_startButton_pressed();
    void on_stopButton_pressed();
    void on_refreshButton_pressed();
    void on_gradient_pressed();
    void on_generations_pressed();
    void on_debug_pressed();
    Gtk::Button startButton;
    Gtk::Button stopButton;
    Gtk::CheckButton showDebugOutput;
    Gtk::CheckButton ignoreGenerations;
    Gtk::CheckButton useGradient;
    Gtk::Entry pointsEntry;
    Gtk::Entry individualsEntry;
    Gtk::Entry treeDepthEntry;
    Gtk::Entry resultEntry;
    Gtk::Entry threadsEntry;
    Gtk::Entry generationsEntry;
    Gtk::Button refreshButton;
    Gtk::Label individualsLabel;
    Gtk::Label pointsLabel;
    Gtk::Label threadsLabel;
    Gtk::Label treeDepthLabel;
    Gtk::Label resultLabel;
    Gtk::Label generationsLabel;
    Gtk::Grid gr;
    Gtk::Grid gr1;
    Gtk::Grid gr2;
    Gtk::Grid gr3;
    Gtk::Grid gr4;
    Gtk::Grid gr5;
    Gtk::Grid gr6;
    thread *thr;
    
};
MainWindow::MainWindow() : startButton("Start"), stopButton("Stop"), refreshButton("Refresh"), individualsLabel("Individuals:"), threadsLabel("Threads:"), treeDepthLabel("Tree depth:"), generationsLabel("Generations:"), pointsLabel("Points:"), resultLabel("Result:"), showDebugOutput("Show debug output"), ignoreGenerations("Ignore generations count"), useGradient("Use gradient descent") {
    set_size_request(600, 600);
    set_title("");
    set_child(gr);
    startButton.set_expand(true);
    stopButton.set_expand(true);
    refreshButton.set_expand(true);
    pointsEntry.set_expand(true);
    individualsEntry.set_expand(true);
    treeDepthEntry.set_expand(true);
    resultEntry.set_expand(true);
    threadsEntry.set_expand(true);
    resultEntry.set_editable(false);
    resultEntry.set_text("Result");
    individualsEntry.set_text("500");
    treeDepthEntry.set_text("3");
    threadsEntry.set_text("1");
    gr.attach(gr1, 0, 0, 1, 1);
    gr1.attach(individualsLabel, 0, 0, 1, 1);
    gr1.attach(individualsEntry, 0, 1, 1, 1);
    gr1.attach(generationsEntry, 0, 3, 1, 1);
    gr1.attach(generationsLabel, 0, 2, 1, 1);
    gr.attach(gr2, 4, 0, 1, 1);
    gr2.attach(treeDepthLabel, 0, 0, 1, 1);
    gr2.attach(treeDepthEntry, 0, 1, 1, 1);
    gr.attach(gr3, 0, 1, 5, 2);
    gr3.attach(pointsLabel, 0, 0, 1, 1);
    gr3.attach(pointsEntry, 0, 1, 1, 1);
    gr.attach(startButton, 0, 3, 2, 2);
    gr.attach(stopButton, 3, 3, 2, 2);
    gr.attach(gr6, 2, 3, 2, 2);
    gr6.attach(showDebugOutput, 0, 0, 1, 1);
    gr6.attach(ignoreGenerations, 0, 1, 1, 1);
    gr6.attach(useGradient, 0, 2, 1, 1);
    gr.attach(gr4, 0, 5, 5, 2);
    gr4.attach(resultEntry, 0, 1, 1, 1);
    //gr4.attach(resultLabel, 0, 0, 1, 1);
    gr.attach(gr5, 2, 0, 1, 1);
    gr5.attach(threadsLabel, 0, 0, 1, 1);
    gr5.attach(threadsEntry, 0, 1, 1, 1);
    gr.attach(refreshButton, 2, 7, 2, 2);
    startButton.signal_clicked().connect(sigc::mem_fun(*this, &MainWindow::on_startButton_pressed));
    stopButton.signal_clicked().connect(sigc::mem_fun(*this, &MainWindow::on_stopButton_pressed));
    refreshButton.signal_clicked().connect(sigc::mem_fun(*this, &MainWindow::on_refreshButton_pressed));
    useGradient.signal_toggled().connect(sigc::mem_fun(*this, &MainWindow::on_gradient_pressed));
    showDebugOutput.signal_toggled().connect(sigc::mem_fun(*this, &MainWindow::on_debug_pressed));
    ignoreGenerations.signal_toggled().connect(sigc::mem_fun(*this, &MainWindow::on_generations_pressed));

    
    
}
void MainWindow::on_generations_pressed(){
    ignore_generations = ignoreGenerations.get_active();
}
void MainWindow::on_debug_pressed(){
    show_debug = showDebugOutput.get_active();
}
void MainWindow::on_gradient_pressed(){
    use_gradient = useGradient.get_active();
}
void MainWindow::on_startButton_pressed() {
    points = {};
    a = {};
    b.clear();
    threads = stoi(threadsEntry.get_text());
    individuals = stoi(individualsEntry.get_text());
    tree_depth = stoi(treeDepthEntry.get_text()); 
    generations = stoi(generationsEntry.get_text());
    string temp = pointsEntry.get_text();
    bool b = true;
    int t = 0;
    vector<double> t1;
    double result;
    for (int i = 0; i < temp.size(); i++) {
        if (temp[i] == ')' || temp[i] == ',') {
            if (b) {
                b = false; 
                result = stof(temp.substr(t, i - t));
                t = i + 1;
            }
            else {
                t1.push_back(stof(temp.substr(t, i - t)));
                t = i + 1;
            }
            
        }
        if (temp[i] == '(') {
            t = i + 1;
        }
        if (temp[i] == ')') {
            points.push_back(Point(result, t1));
            variables = t1.size();
            t1 = {};
            b = true;
        }
    }
    thr = new thread(start);
   
};
void MainWindow::on_stopButton_pressed() {
    exit(0);
};
void MainWindow::on_refreshButton_pressed() {
    resultEntry.set_text(best->dna->print_tree());
};
    int main(int argc, char* argv[]) {
    auto app = Gtk::Application::create();
    return app->make_window_and_run<MainWindow>(argc, argv);
}

