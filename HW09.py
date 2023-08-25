# import packages - random to specify probability distributions
import random
import simpy

## define system contraints
num_checkers = 10
num_scanners = 25
r_seed = 500

# define rates
arrival_rate = 5 # rate is passengers per minute
check_rate = .75 # rate is minutes per passenger
min_scan = .5 # scanner min time - uniform distribution
max_scan = 1.0 # scanner max time - uniform distribution

# define simulation timing
run_time = 100 # minutes per simulation
reps = 100 # number of simulation replications

# initalize global variables - will store answers in these variables later
checkWait = 0
scanWait = 0
sysTime = 0
timeWait = 0
timeChecker = 0
timeCheckerComplete = 0
timeScan = 0 
timeScanComplete = 0
passenger_count = 0

# create the simulation object - airport includes checkers and scanners
class Airport(object):
    # define the overall resources in our system - checkers and scanners
    def __init__(self, env):
        self.env = env
        self.checker = simpy.Resource(env, num_checkers)
        self.scanners = []
        # for each scanner - assign a resource - multiple scanners with 1 resource
        for i in range(0,25):
            resource = simpy.Resource(env, capacity = 1)
            self.scanners.append(resource)

    # define how long a passenger gets to the checkout
    def check(self, passenger):
        yield self.env.timeout(random.expovariate(1/check_rate))

    # define how long a passenger takes to scan themselves in
    def scan(self, passenger):
        yield self.env.timeout(random.uniform(max_scan , min_scan))


# define attributes of a passenger - how a passenger moves through the system (checkers, scanners)
def passenger(env, name, s): # enviroment, name, system reference

    # initalize global variables - will store answers in these variables later
    global checkWait
    global scanWait
    global sysTime
    global timeWait
    global timeChecker
    global timeCheckerComplete
    global timeScan
    global timeScanComplete
    global passenger_count

    # time passenger arrives
    timeArrive = env.now
    print('%s arrives at time %.2f' % (name, timeArrive))

    with s.checker.request() as request:
        yield request
        print('check queue length = %d' % len(s.checker.queue))

        # time passenger arrives at checker
        timeChecker = env.now
        print('%s gets to checker at time %.2f' % (name, timeChecker))

        yield env.process(s.check(name))

        # time passenger completes checker
        timeCheckerComplete = env.now
        print('%s complete checker at time %.2f' % (name, timeCheckerComplete))

    min_q = 0

    with s.scanners[min_q].request() as request:
        yield request
        print('scanner queue length = %d' % len(s.scanners[min_q].queue))

        for i in range(1, num_scanners):
            if (len(s.scanners[i].queue) < len(s.scanners[min_q].queue)):
                min_q = i

        # time passenger arrives at scanner
        timeScan = env.now
        print('%s gets to scanner at time %.2f' % (name, timeScan))

        yield env.process(s.scan(name))

        timeScanComplete = env.now
        print('%s complete scanner at time %.2f' % (name, timeScanComplete))


    # time at the end of entire system process
    timeExit = env.now
    print('%s gets to complete system time %.2f' % (name, timeExit))
    
    sysTime = sysTime + (timeExit - timeArrive)
    checkWait = checkWait + (timeChecker - timeArrive)
    scanWait = scanWait + (timeScanComplete - timeCheckerComplete)
    timeWait = (checkWait + scanWait)


def setup(env):
    airport = Airport(env)
    i = 0
    while True:
        yield env.timeout(random.expovariate(1.0 / check_rate))
        i += 1
        env.process(passenger(env, 'Passenger %d' % i, airport))


# define variables to store simulation results into
avg_wait_time = []
avg_check_time = []
avg_scan_time = []
avg_sys_time = []

for i in range(0, reps):
    env = simpy.Environment()
    env.process(setup(env))
    env.run(until = run_time)
    
    avg_wait_time.append(timeWait) 
    avg_check_time.append(checkWait)
    avg_scan_time.append(scanWait)
    avg_sys_time.append(sysTime)
    
    passenger_count = 0
    sysTime = 0
    checkWait = 0
    scanWait = 0
    timeWait = 0
    
    
sim_wait_avg = sum(avg_wait_time) / reps
sim_check_avg = sum(avg_check_time) / reps
sim_scan_avg = sum(avg_scan_time) / reps
sim_sys_time = sum(avg_sys_time) / reps


print('Average cummulative wait time: ' + str(sim_wait_avg))
print('Average cummulative check time: ' + str(sim_check_avg))
print('Average cummulative scan time: ' + str(sim_scan_avg))
print('Average cummulative system time: ' + str(sim_sys_time))